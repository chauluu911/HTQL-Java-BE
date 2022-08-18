package com.evotek.meet.application.service;

import com.evotek.meet.application.dto.request.UserSchedulerRequest;
import com.evotek.meet.application.dto.response.UserSchedulerResponse;
import com.evotek.meet.domain.UserScheduler;

import java.util.List;

public interface UserService {
    //List<UserScheduler> findAll();

    List<UserSchedulerResponse> getScheduler(UserSchedulerRequest request);

    UserScheduler ensureExisted(String id);

    void reject(String userSchedulerId);
}
