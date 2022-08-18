package com.evotek.meet.application.service;

import com.evotek.common.dto.PageDTO;
import com.evotek.common.webapp.support.DomainService;
import com.evotek.meet.application.dto.request.MeetingCreateOrUpdateRequest;
import com.evotek.meet.application.dto.request.MeetingSearchRequest;
import com.evotek.meet.domain.Meeting;

public interface MeetingService extends DomainService<Meeting, String> {

    Meeting create(MeetingCreateOrUpdateRequest request);

    Meeting update(String id, MeetingCreateOrUpdateRequest request);

    void delete(String id);

    Meeting ensureExisted(String id);

    Meeting findByMeetingId(String id);

    PageDTO<Meeting> search(MeetingSearchRequest searchRequest);

    Meeting approve(String id);

    Meeting reject(String id);

    void cancel(String id);
}
