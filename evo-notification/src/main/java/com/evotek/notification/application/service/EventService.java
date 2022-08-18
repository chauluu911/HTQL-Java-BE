package com.evotek.notification.application.service;

import com.evotek.common.dto.PageDTO;
import com.evotek.common.dto.request.notification.IssueEventRequest;
import com.evotek.common.webapp.support.DomainService;
import com.evotek.notification.application.dto.request.EventCreateRequest;
import com.evotek.notification.application.dto.request.EventSearchRequest;
import com.evotek.notification.application.dto.request.EventUpdateRequest;
import com.evotek.notification.domain.Event;

public interface EventService extends DomainService<Event, String> {
    /**
     * Tạo event và bắn notification toi device of user
     *
     * @param request
     * @return
     */
    Event create(EventCreateRequest request);

    /**
     * chi duoc phep cap nhat nhung event o trang thai waiting
     *
     * @param request
     * @return
     */
    Event update(String id, EventUpdateRequest request);

    Event getById(String id);

    /**
     * search event
     *
     * @param searchRequest
     * @return
     */
    PageDTO<Event> search(EventSearchRequest searchRequest);

    /**
     * Cancel event which is waiting
     *
     * @param uuid
     */
    void cancel(String uuid);

    void send(String id);

    void delete(String id);

    Event issued(IssueEventRequest request);

}
