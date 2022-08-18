package com.evotek.meet.application.service.impl;

import com.evotek.common.client.iam.IAMClient;
import com.evotek.common.client.notification.NotificationClient;
import com.evotek.common.client.storage.StorageClient;
import com.evotek.common.client.system.SystemClient;
import com.evotek.common.dto.request.FindByIdsRequest;
import com.evotek.common.dto.request.notification.IssueEventRequest;
import com.evotek.common.dto.response.Response;
import com.evotek.common.dto.response.iam.UserDTO;
import com.evotek.common.dto.response.system.ConfigurationDTO;
import com.evotek.common.enums.ConfigurationType;
import com.evotek.common.enums.EventType;
import com.evotek.common.enums.TargetType;
import com.evotek.common.exception.ResponseException;
import com.evotek.common.util.DateUtils;
import com.evotek.common.util.StringPool;
import com.evotek.common.webapp.i18n.LocaleStringService;
import com.evotek.meet.application.service.SendService;
import com.evotek.meet.application.service.ThymeleafService;
import com.evotek.meet.domain.*;
import com.evotek.meet.domain.repository.MeetingDomainRepository;
import com.evotek.meet.domain.repository.RoomDomainRepository;
import com.evotek.meet.domain.repository.UserSchedulerDomainRepository;
import com.evotek.meet.infrastructure.persistence.entity.MeetingAttendeeEntity;
import com.evotek.meet.infrastructure.persistence.entity.UserSchedulerEntity;
import com.evotek.meet.infrastructure.persistence.mapper.MeetingAttendeeEntityMapper;
import com.evotek.meet.infrastructure.persistence.mapper.UserSchedulerEntityMapper;
import com.evotek.meet.infrastructure.persistence.repository.MeetingAttendeeEntityRepository;
import com.evotek.meet.infrastructure.persistence.repository.UserSchedulerEntityRepository;
import com.evotek.meet.infrastructure.support.Util.Const;
import com.evotek.meet.infrastructure.support.enums.ApproveStatus;
import com.evotek.meet.infrastructure.support.enums.MeetingStatus;
import com.evotek.meet.infrastructure.support.exception.BadRequestError;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.fileupload.disk.DiskFileItem;
import org.apache.commons.io.IOUtils;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.multipart.commons.CommonsMultipartFile;

import java.io.*;
import java.text.SimpleDateFormat;
import java.time.Instant;
import java.time.ZoneId;
import java.util.*;
import java.util.stream.Collectors;

@Service
@Slf4j
public class SendServiceImpl implements SendService {

    private static final String TEMPLATE_NAME = "mail/mailInvitationAttend";
    private final MeetingAttendeeEntityRepository meetingAttendeeEntityRepository;
    private final ThymeleafService thymeleafService;
    private final NotificationClient notificationClient;
    private final LocaleStringService localeStringService;
    private final MeetingDomainRepository meetingDomainRepository;
    private final RoomDomainRepository roomDomainRepository;
    private final UserSchedulerEntityRepository userSchedulerEntityRepository;
    private final UserSchedulerDomainRepository userSchedulerDomainRepository;
    private final UserSchedulerEntityMapper userSchedulerEntityMapper;
    private final MeetingAttendeeEntityMapper meetingAttendeeEntityMapper;
    private final IAMClient iamClient;
    private final SystemClient systemClient;
    private final StorageClient storageClient;

    private static final String calendarTimeZoneType = "SE Asia Standard Time";

    public SendServiceImpl(MeetingAttendeeEntityRepository meetingAttendeeEntityRepository, ThymeleafService thymeleafService, NotificationClient notificationClient, LocaleStringService localeStringService, MeetingDomainRepository meetingDomainRepository, RoomDomainRepository roomDomainRepository, UserSchedulerEntityRepository userSchedulerEntityRepository, UserSchedulerDomainRepository userSchedulerDomainRepository, UserSchedulerEntityMapper userSchedulerEntityMapper, MeetingAttendeeEntityMapper meetingAttendeeEntityMapper, IAMClient iamClient, SystemClient systemClient, StorageClient storageClient) {
        this.meetingAttendeeEntityRepository = meetingAttendeeEntityRepository;
        this.thymeleafService = thymeleafService;
        this.notificationClient = notificationClient;
        this.localeStringService = localeStringService;
        this.meetingDomainRepository = meetingDomainRepository;
        this.roomDomainRepository = roomDomainRepository;
        this.userSchedulerEntityRepository = userSchedulerEntityRepository;
        this.userSchedulerDomainRepository = userSchedulerDomainRepository;
        this.userSchedulerEntityMapper = userSchedulerEntityMapper;
        this.meetingAttendeeEntityMapper = meetingAttendeeEntityMapper;
        this.iamClient = iamClient;
        this.systemClient = systemClient;
        this.storageClient = storageClient;
    }

    @Override
    public void sendMail(String id) {
        if (Objects.isNull(id)) {
            throw new ResponseException(String.format(BadRequestError.INVALID_INPUT.getMessage(), "id"),
                    BadRequestError.INVALID_INPUT);
        }
        Meeting meeting = this.meetingDomainRepository.getById(id);
        if (Objects.isNull(meeting)) {
            throw new ResponseException(BadRequestError.MEETING_USER_INVALID);
        }
        // send attachment
        MultipartFile multipartFileInvite = transferFileToMultipartFile(createFileInviteCalendar(meeting));
        String idFile = this.storageClient.fileUpload(multipartFileInvite, meeting.getId(), Const.MEET_OWNER_TYPE).getData().getId();
//        log.info(idFile);
        Response<List<UserDTO>> responseUsers = iamClient.findByUserIds(new FindByIdsRequest(List.of(meeting.getOrganizerId())));
        UserDTO userDTO = responseUsers.getData().get(0);
        List<String> userIds = this.meetingAttendeeEntityRepository.findByMeetingId(id).stream()
                .map(MeetingAttendeeEntity::getUserId)
                .collect(Collectors.toList());
        IssueEventRequest issueEventRequest = IssueEventRequest.builder()
                .title(meeting.getTitle())
                .types(Set.of(EventType.EMAIL))
                .content(this.thymeleafService.getContent(TEMPLATE_NAME, meeting.getId(), userDTO.getFullName()))
                .targets(userIds)
                .targetType(TargetType.USER)
                .attachmentFileIds(List.of(idFile))
                .build();
        notificationClient.issueEvent(issueEventRequest);
    }

    public MultipartFile transferFileToMultipartFile(File file) {
        DiskFileItem fileItem = new DiskFileItem("file", "text/plain", false, file.getName(), (int) file.length(), file.getParentFile());
        InputStream inputStream = null;
        try {
            inputStream = new FileInputStream(file);
            OutputStream outputStream = fileItem.getOutputStream();
            IOUtils.copy(inputStream, outputStream);
        } catch (IOException e) {
            throw new RuntimeException(e);
        } finally {
            try {
                if (inputStream != null) {
                    inputStream.close();
                }        
            } catch (IOException e) {
                // handle an exception, or often we just ignore it
            }
        }
        MultipartFile multipartFile = new CommonsMultipartFile(fileItem);

        return multipartFile;
    }

    public File createFileInviteCalendar(Meeting meeting) {
        StringBuilder result = new StringBuilder();
        result.append("BEGIN:VCALENDAR\n")
                .append("PRODID:-//Microsoft Corporation//Outlook 16.0 MIMEDIR//EN\n")
                .append("METHOD:REQUEST\n")
                .append("VERSION:2.0\n")
                .append("X-MS-OLK-FORCEINSPECTOROPEN:TRUE\n")
                .append("BEGIN:VTIMEZONE\n")
                .append("TZID:").append(calendarTimeZoneType).append("\n")
                .append("BEGIN:STANDARD\n")
                .append("DTSTART:16010101T000000\n")
                .append("TZOFFSETFROM:+0700\n")
                .append("TZOFFSETTO:+0700\n")
                .append("END:STANDARD\n")
                .append("END:VTIMEZONE\n")
                // Event Time and location
                .append("BEGIN:VEVENT\n");
        // User attend
        List<String> userIds = meeting.getMeetingAttendees().stream().map(MeetingAttendee::getUserId).collect(Collectors.toList());
        if(!userIds.isEmpty())
        {
            Response<List<UserDTO>> users = this.iamClient.findByUserIds(new FindByIdsRequest(userIds));
            users.getData().forEach(user -> {
                result.append("ATTENDEE;CN=\"")
                        .append(user.getFullName())
                        .append("\";RSVP=TRUE:mailto:")
                        .append(user.getEmail()).append("\n");
            });
        }
        result.append("CLASS:PUBLIC\n")
                .append("CREATED:").append(meeting.getCreatedAt().toString()).append("\n")
                .append("DESCRIPTION:").append(meeting.getDescription()).append("\n")
                .append("DTEND;TZID=\"").append(calendarTimeZoneType).append("\":")
                .append(meeting.getFinishAt().toString()).append("\n")
                .append("DTSTAMP:").append(Instant.now().toString()).append("\n")
                .append("DTSTART;TZID=\"").append(calendarTimeZoneType).append("\":")
                .append(meeting.getStartAt().toString()).append("\n")
                .append("LAST-MODIFIED:").append(meeting.getLastModifiedAt().toString()).append("\n")
                .append("LOCATION:").append(meeting.getRoom().getName()).append("\n");
        //Get PresiderUser
        List<UserDTO> presideUsers = this.iamClient.findByUserIds(new FindByIdsRequest(List.of(meeting.getPresiderId()))).getData();
        if (!presideUsers.isEmpty()) {
            UserDTO presideUser = presideUsers.get(0);
            result.append("ORGANIZER;CN=")
                    .append(presideUser.getEmail())
                    .append(":mailto:").append(presideUser.getEmail())
                    .append("\n");
        }
        result.append("PRIORITY:5\n").append("SEQUENCE:0\n").append("SUMMARY;LANGUAGE=en-us:").append(meeting.getTitle()).append("\n")
                .append("TRANSP:OPAQUE\n").append("UID:").append(UUID.randomUUID()).append("\n")
                .append("X-MICROSOFT-CDO-BUSYSTATUS:TENTATIVE\n")
                .append("X-MICROSOFT-CDO-IMPORTANCE:1\n")
                .append("X-MICROSOFT-CDO-INTENDEDSTATUS:BUSY\n")
                .append("X-MICROSOFT-DISALLOW-COUNTER:FALSE\n")
                .append("X-MS-OLK-AUTOSTARTCHECK:FALSE\n")
                .append("X-MS-OLK-CONFTYPE:0\n")
                // Remind before 15 minutes
                .append("BEGIN:VALARM\n")
                .append("TRIGGER:-PT15M\n")
                .append("ACTION:DISPLAY\n")
                .append("DESCRIPTION:Reminder\n")
                .append("END:VALARM\n")
                .append("END:VEVENT\n")
                .append("END:VCALENDAR");
        String resultFile = String.format("invite%s", DateUtils.format(new Date(), "yyyyMMddHHmmss"));
        File fileInvite = new File(System.getProperty("java.io.tmpdir") + StringPool.SLASH + resultFile + StringPool.ICS);
        FileOutputStream fileOutputStream = null;
        try {
            boolean fileCreated = fileInvite.createNewFile();
            // Validate that file actually got created
            if (!fileCreated) {
                throw new IOException("Unable to create file at specified path. It already exists");
            }
            fileOutputStream = new FileOutputStream(fileInvite);
            fileOutputStream.write(result.toString().getBytes());
            fileOutputStream.flush();
            //fileOutputStream.close();
        } catch (IOException e) {
            throw new RuntimeException(e);
        } finally {
            try {
                if (fileOutputStream != null) {
                    fileOutputStream.close();
                }        
            } catch (IOException e) {
                // handle an exception, or often we just ignore it
            }
        }
        return fileInvite;

    }

    @Override
    public void sendNotification(String id) {
        if (Objects.isNull(id)) {
            throw new ResponseException(String.format(BadRequestError.INVALID_INPUT.getMessage(), "id"),
                    BadRequestError.INVALID_INPUT);
        }
        Meeting meeting = this.meetingDomainRepository.getById(id);
        Response<List<UserDTO>> responseUsers = iamClient.findByUserIds(new FindByIdsRequest(List.of(meeting.getOrganizerId())));
        UserDTO userDTO = responseUsers.getData().get(0);
        List<String> userIds = this.meetingAttendeeEntityRepository.findByMeetingId(id).stream()
                .map(MeetingAttendeeEntity::getUserId)
                .collect(Collectors.toList());
        SimpleDateFormat formatDateTime = new SimpleDateFormat("dd-MM-yyyy HH:mm");
        SimpleDateFormat formatTime = new SimpleDateFormat("HH:mm");
        String startAt = formatDateTime.format(Date.from(meeting.getStartAt().atZone(ZoneId.of("Asia/Ho_Chi_Minh")).toInstant()));
        String finishTime = formatTime.format(Date.from(meeting.getFinishAt().atZone(ZoneId.of("Asia/Ho_Chi_Minh")).toInstant()));
        String content = String.format("%s%n%s%n%s%s", localeStringService.getMessage("INVITATION_JOIN_MEETING", "", userDTO.getFullName()),
                localeStringService.getMessage("NOTIFICATION_JOIN_MEETING", "", meeting.getTitle()),
                localeStringService.getMessage("START_AT", "", startAt), " - " + finishTime);
        IssueEventRequest issueEventRequest = IssueEventRequest.builder()
                .title(meeting.getTitle())
                .types(Set.of(EventType.NOTIFICATION))
                .content(content)
                .targets(userIds)
                .targetType(TargetType.USER)
                .build();
        notificationClient.issueEvent(issueEventRequest);
    }

    @Override
    public void sendUserScheduler(String id) {
        Meeting meeting = this.meetingDomainRepository.getById(id);
        if (meeting.getMeetingStatus().equals(MeetingStatus.ACTIVE) && Boolean.FALSE.equals(meeting.getDeleted())) {
            List<String> userIds = new ArrayList<>();
            SimpleDateFormat formatDateTime = new SimpleDateFormat("HH:mm dd-MM-yyyy");
            Response<ConfigurationDTO> response = this.systemClient.findByCode(ConfigurationType.TELEGRAM_USER_SCHEDULER);
            ConfigurationDTO configuration = response.getData();
            String startAtUTC = formatDateTime.format(Date.from(Instant.now().plusSeconds(Long.parseLong(configuration.getValue()))));
            List<UserSchedulerEntity> userSchedulerEntities = this.userSchedulerEntityRepository.findAllByMeetingId(id);
            List<UserScheduler> userSchedulers = this.userSchedulerEntityMapper.toDomain(userSchedulerEntities);
            for (UserScheduler userScheduler : userSchedulers) {
                String userSchedulerStartAt = formatDateTime.format(Date.from(userScheduler.getStartAt()));
                if (!userIds.contains(userScheduler.getUserId()) && userScheduler.getApproveStatus().equals(ApproveStatus.APPROVED)
                        && Boolean.FALSE.equals(userScheduler.getDeleted()) && startAtUTC.equals(userSchedulerStartAt)) {
                    userIds.add(userScheduler.getUserId());
                }
            }

            String content = "";
            String startAt = formatDateTime.format(Date.from(Instant.now().atZone(ZoneId.of("Asia/Ho_Chi_Minh")).toInstant().plusSeconds(Long.parseLong(configuration.getValue()))));
            if (!Objects.isNull(meeting.getRoomId())) {
                Room room = this.roomDomainRepository.getById(meeting.getRoomId());
                content = String.format("%s%n%s%n%s", localeStringService.getMessage("NOTIFICATION_JOIN_MEETING", "", meeting.getTitle()),
                        localeStringService.getMessage("LOCATION", "", room.getLocation() + "-" + room.getName()),
                        localeStringService.getMessage("START_AT", "", startAt));
            } else {
                content = String.format("%s%n%s%n%s", localeStringService.getMessage("NOTIFICATION_JOIN_MEETING", "", meeting.getTitle()),
                        localeStringService.getMessage("LINK", "", meeting.getLink()),
                        localeStringService.getMessage("START_AT", "", startAt));
            }
            IssueEventRequest issueEventRequest = IssueEventRequest.builder()
                    .title(meeting.getTitle())
                    .types(Set.of(EventType.NOTIFICATION))
                    .content(content)
                    .targets(userIds)
                    .targetType(TargetType.USER)
                    .build();
            notificationClient.issueEvent(issueEventRequest);
        }
    }

    @Override
    public void sendRejectOrCancelUserScheduler(UserSchedulerRejectOrCancelTaskData data) {
        SimpleDateFormat formatDateTime = new SimpleDateFormat("dd-MM-yyyy HH:mm");
        SimpleDateFormat formatTime = new SimpleDateFormat("HH:mm");
        UserScheduler userScheduler = this.userSchedulerDomainRepository.getById(data.getUserSchedulerId());
        Meeting meeting = this.meetingDomainRepository.getById(data.getMeetingId());
        List<String> userIds = new ArrayList<>();
        if (Boolean.TRUE.equals(data.getIsOrganizer())) {
            List<MeetingAttendee> meetingAttendees = meetingAttendeeEntityMapper.toDomain(
                    meetingAttendeeEntityRepository.findByMeetingId(meeting.getId()));
            userIds = meetingAttendees.stream().filter(item -> !Objects.equals(data.getCurrentUserId(), item.getUserId()))
                    .map(MeetingAttendee::getUserId).collect(Collectors.toList());
        } else {
            userIds.add(meeting.getOrganizerId());
        }
        Response<List<UserDTO>> responseUsers = iamClient.findByUserIds(new FindByIdsRequest(List.of(data.getCurrentUserId())));
        UserDTO userDTO = responseUsers.getData().get(0);
        String startAt = formatDateTime.format(Date.from(userScheduler.getStartAt().atZone(ZoneId.of("Asia/Ho_Chi_Minh")).toInstant()));
        String finishAt = formatDateTime.format(Date.from(userScheduler.getFinishAt().atZone(ZoneId.of("Asia/Ho_Chi_Minh")).toInstant()));
        String contentCurrentUser = "";
        String contentOrganizer = "";
        if (Boolean.TRUE.equals(data.getIsReject())) {
            contentCurrentUser = String.format("%s%n%s%n%s%n%s", localeStringService.getMessage("REJECT", "", "Bạn"), localeStringService.getMessage("MEETING", "", meeting.getTitle()),
                    localeStringService.getMessage("START_AT", "", startAt), localeStringService.getMessage("FINISH_AT", "", finishAt));
            contentOrganizer = String.format("%s%n%s%n%s%n%s", localeStringService.getMessage("REJECT", "", userDTO.getFullName()), localeStringService.getMessage("MEETING", "", meeting.getTitle()),
                    localeStringService.getMessage("START_AT", "", startAt), localeStringService.getMessage("FINISH_AT", "", finishAt));
        }

        if (Boolean.TRUE.equals(data.getIsCancel())) {
            String finishTime = formatTime.format(Date.from(meeting.getFinishAt().atZone(ZoneId.of("Asia/Ho_Chi_Minh")).toInstant()));
            contentCurrentUser = String.format("%s%n%s%n%s%s%n%s", localeStringService.getMessage("REJECT", "", "Bạn"), localeStringService.getMessage("MEETING", "", meeting.getTitle()),
                    localeStringService.getMessage("START", "", startAt), " - " + finishTime, localeStringService.getMessage("END", "", meeting.getEndDate()));
            contentOrganizer = String.format("%s%n%s%n%s%s%n%s", localeStringService.getMessage("REJECT", "", userDTO.getFullName()), localeStringService.getMessage("MEETING", "", meeting.getTitle()),
                    localeStringService.getMessage("START", "", startAt), " - " + finishTime, localeStringService.getMessage("END", "", meeting.getEndDate()));
        }

        IssueEventRequest issueEventCurrentUser = IssueEventRequest.builder()
                .title(meeting.getTitle())
                .types(Set.of(EventType.NOTIFICATION))
                .content(contentCurrentUser)
                .targets(List.of(data.getCurrentUserId()))
                .targetType(TargetType.USER)
                .build();
        notificationClient.issueEvent(issueEventCurrentUser);
        IssueEventRequest issueEventOrganizer = IssueEventRequest.builder()
                .title(meeting.getTitle())
                .types(Set.of(EventType.NOTIFICATION))
                .content(contentOrganizer)
                .targets(userIds)
                .targetType(TargetType.USER)
                .build();
        notificationClient.issueEvent(issueEventOrganizer);
    }

    @Override
    public void sendApproveUserScheduler(UserSchedulerApproveTaskData data) {
        Meeting meeting = this.meetingDomainRepository.getById(data.getMeetingId());
        SimpleDateFormat formatDateTime = new SimpleDateFormat("dd-MM-yyyy HH:mm");
        SimpleDateFormat formatTime = new SimpleDateFormat("HH:mm");
        Response<List<UserDTO>> responseUsers = iamClient.findByUserIds(new FindByIdsRequest(List.of(data.getCurrentUserId())));
        UserDTO userDTO = responseUsers.getData().get(0);
        String startAt = formatDateTime.format(Date.from(meeting.getStartAt().atZone(ZoneId.of("Asia/Ho_Chi_Minh")).toInstant()));
        String finishTime = formatTime.format(Date.from(meeting.getFinishAt().atZone(ZoneId.of("Asia/Ho_Chi_Minh")).toInstant()));
        String content = String.format("%s%n%s%n%s%s%n%s", localeStringService.getMessage("APPROVE", "", userDTO.getFullName()), localeStringService.getMessage("MEETING", "", meeting.getTitle()),
                localeStringService.getMessage("START_AT", "", startAt), " - " + finishTime, localeStringService.getMessage("END_DATE", "", meeting.getEndDate()));
        IssueEventRequest issueEventRequest = IssueEventRequest.builder()
                .title(meeting.getTitle())
                .types(Set.of(EventType.NOTIFICATION))
                .content(content)
                .targets(List.of(meeting.getOrganizerId()))
                .targetType(TargetType.USER)
                .build();
        notificationClient.issueEvent(issueEventRequest);
    }

}
