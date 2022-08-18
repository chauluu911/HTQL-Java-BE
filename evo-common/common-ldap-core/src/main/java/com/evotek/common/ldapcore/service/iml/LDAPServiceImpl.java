package com.evotek.common.ldapcore.service.iml;

import com.evotek.common.exception.ResponseException;
import com.evotek.common.ldapcore.config.LDAPProperties;
import com.evotek.common.ldapcore.dto.request.UserLdapChangePasswordRequest;
import com.evotek.common.ldapcore.dto.request.UserLdapCreateOrUpdateRequest;
import com.evotek.common.ldapcore.dto.response.UserLdapResponse;
import com.evotek.common.ldapcore.service.LDAPService;
import com.evotek.common.ldapcore.support.exception.BadRequestError;
import com.evotek.common.ldapcore.support.ssl.BlindSslSocketFactory;
import com.evotek.common.ldapcore.support.util.Const;
import com.evotek.common.util.StrUtils;
import com.evotek.common.util.StringPool;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.nimbusds.jose.shaded.json.JSONObject;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.naming.AuthenticationException;
import javax.naming.Context;
import javax.naming.NamingEnumeration;
import javax.naming.NamingException;
import javax.naming.directory.*;
import javax.naming.ldap.*;
import java.io.IOException;
import java.util.*;
import java.util.stream.Collectors;

@Service
@Slf4j
@EnableConfigurationProperties(LDAPProperties.class)
public class LDAPServiceImpl implements LDAPService {

    private final LDAPProperties ldapProperties;
    private final ObjectMapper objectMapper;

    public LDAPServiceImpl(LDAPProperties ldapProperties, ObjectMapper objectMapper) {
        this.ldapProperties = ldapProperties;
        this.objectMapper = objectMapper;
    }

    @Override
    public Boolean authenticateUserLDAP(String username, String password) {
        log.info("Start authenticate user with normal ldap account: {}", username);
        LdapContext ctxGC = initLdapContext(String.format("%s%s", ldapProperties.getFilteredGroup().getPrefix(), username), password);
        return Objects.nonNull(ctxGC);
    }

    private LdapContext initLdapContext(String username, String password) {
        // Using standard Port, check your installation
        System.setProperty("com.sun.jndi.ldap.object.disableEndpointIdentification", "true");
        // Init context Ldap with admin account
        Hashtable<String, String> environment = new Hashtable<>();
        environment.put(Context.INITIAL_CONTEXT_FACTORY, "com.sun.jndi.ldap.LdapCtxFactory");
        environment.put(Context.PROVIDER_URL, ldapProperties.getUrls());
        environment.put(Context.SECURITY_AUTHENTICATION, "simple");
        environment.put(Context.SECURITY_PRINCIPAL, username);
        environment.put(Context.SECURITY_CREDENTIALS, password);
        if (ldapProperties.getUrls().startsWith(Const.LDAPS)) {
            environment.put(Context.SECURITY_PROTOCOL, "ssl");
            environment.put("java.naming.ldap.factory.socket", BlindSslSocketFactory.class.getName());
        }
        environment.put("java.naming.referral", "follow");

        LdapContext ctxGC = null;
        try {
            ctxGC = new InitialLdapContext(environment, null);
            ctxGC.getAttributes("");
        } catch (AuthenticationException e) {
            log.error("Authenticate ldap account fail, user: " + username, e);
        } catch (NamingException namingException) {
            log.error("Searcher blocked user: " + username, namingException);
        } catch (Exception e) {
            log.error("Searcher wrong password, user: " + username, e);
        }
        log.info("Finish authenticate user to init ldap context with user: {}", username);
        return ctxGC;
    }

    @Override
    public Boolean authenticateUserLDAPInGroup(String username, String password) {
        log.info("Start authenticate user with ldap account in group!");
        LdapContext ctxGC = initLdapContext(String.format("%s%s", ldapProperties.getFilteredGroup().getPrefix(), username), password);
        log.info("Ldap Context ctxGC: {}", ctxGC);
        log.info("Valid group: {}", Arrays.asList(ldapProperties.getFilteredGroup().getGroups()));
        if (ctxGC != null) {
            List<String> groups = getGroupsOfUser(ctxGC, username);
            for (String group : groups) {
                boolean isAccountInGroup;
                isAccountInGroup = Arrays.stream(ldapProperties.getFilteredGroup().getGroups()).anyMatch(g -> g.equalsIgnoreCase(group));
                if (Boolean.TRUE.equals(isAccountInGroup)) {
                    log.info("User {} is in valid group {}", username, group);
                    return true;
                }
            }
        }
        log.info("User {} is not in valid group", username);
        return false;
    }

    public List<String> getGroupsOfUser(LdapContext ctxGC, String username) {
        List<String> groups = new ArrayList<>();
        // Search user in group
        try {
            // filter search
            String searchFilter = String.format("(&(objectclass=user)(sAMAccountName=%s))", username);
            String[] returnedAttrs = {MEMBER_OF_ATTRIBUTE};

            // Create the search controls
            SearchControls searchControls = new SearchControls();
            searchControls.setReturningAttributes(returnedAttrs);
            searchControls.setSearchScope(SearchControls.ONELEVEL_SCOPE);

            NamingEnumeration<?> answer = ctxGC.search(ldapProperties.getFilteredGroup().getBaseDnGroup(), searchFilter, searchControls);

            // get group by memberOf
            while (answer.hasMore()) {
                SearchResult result = (SearchResult) answer.next();
                Attributes attrs = result.getAttributes();
                String groupAttrs = attrs.get(MEMBER_OF_ATTRIBUTE).toString();
                String groupNames = groupAttrs.split(StringPool.COLON)[1].trim();
                log.info("Group name of user {}:  {}", username, groupNames);

                String[] commonGroups = groupNames.split(StringPool.COMMA);
                for (String groupName : commonGroups) {
                    if (COMMON_NAME.equalsIgnoreCase(groupName.split(StringPool.EQUAL)[0])) {
                        groups.add(groupName.split(StringPool.EQUAL)[1]);
                    }
                }
            }

            // close connection context
            ctxGC.close();
            log.info("All groups of user {}:  {}", username, groups);
            return groups;
        } catch (Exception e) {
            log.error("Search user with username in group: {} fail!!", username);
        }
        return new ArrayList<>();
    }

    @Override
    public List<UserLdapResponse> getAllUserInLdap() {
        log.info("Start get all user in group ldap!");
        LdapContext ctxGC = initLdapContext(String.format("%s%s", ldapProperties.getFilteredGroup().getPrefix(),
                ldapProperties.getUsername()), ldapProperties.getPassword());
        if (ctxGC == null) {
            log.info("Init LDAP context error");
            return new ArrayList<>();
        }
        SearchControls searchControls = new SearchControls();
        searchControls.setSearchScope(SearchControls.SUBTREE_SCOPE);

        // filter
        String filterBy = "(&(objectClass=user)(sAMAccountName=*))";

        try {
            NamingEnumeration<?> objs = ctxGC.search(ldapProperties.getFilteredGroup().getBaseDnGroup(), filterBy, searchControls);
            List<UserLdapResponse> userLdapResponses;

            List<String> jsonElements = this.getJsonElements(objs);

            userLdapResponses = this.parseJsonElementToUserLdap(jsonElements);

            return userLdapResponses.stream()
                    .filter(user -> !StrUtils.isBlank(user.getUserPrincipalName()) &&
                            user.getUserPrincipalName().contains(ldapProperties.getPrefixMail()))
                    .collect(Collectors.toList());
        } catch (NamingException namingException) {
            log.error("Error with namingException " + namingException);
        }
        return new ArrayList<>();
    }

    @Override
    @Transactional
    public void createUserLdap(UserLdapCreateOrUpdateRequest request) {
        LdapContext ctxGC = initLdapContext(String.format("%s%s", ldapProperties.getFilteredGroup().getPrefix(),
                ldapProperties.getUsername()), ldapProperties.getPassword());

        if (Objects.isNull(ctxGC)) {
            throw new ResponseException(BadRequestError.CONNECT_LDAP_FAIL);
        }

        // Create a container set of attributes
        Attributes container = new BasicAttributes();

        // Create the objectclass to add
        Attribute objClasses = new BasicAttribute("objectClass");
        objClasses.add("top");
        objClasses.add("person");
        objClasses.add("organizationalPerson");
        objClasses.add("user");

        // Assign the username, first name, and last name
        String lastName = request.getFullName();
        Attribute cn = new BasicAttribute("cn", request.getFullName());
        Attribute sAMAccountName = new BasicAttribute("sAMAccountName", request.getUsername());
        Attribute principalName = new BasicAttribute("userPrincipalName", request.getUsername() + StringPool.AT + ldapProperties.getDomain());
        Attribute sn = new BasicAttribute("sn", lastName);
        Attribute uid = new BasicAttribute("uid", request.getUsername());

        // Add password
        Attribute userPassword = new BasicAttribute("unicodePwd", encodePassword(request.getPassword()));

        // Add these to the container
        container.put(objClasses);
        container.put(sAMAccountName);
        container.put(principalName);
        container.put(cn);
        container.put(sn);
        container.put(uid);
        container.put(userPassword);

        // Create the entry
        try {
            ctxGC.createSubcontext(getUserDN(request.getFullName()), container);
        } catch (Exception e) {
            log.error("Error to create entry user {}", request.getFullName());
            throw new ResponseException(BadRequestError.CREATE_USER_LDAP_FAIL);
        }
    }

    @Override
    public boolean resetPassUserLdap(String cn, UserLdapChangePasswordRequest request) {
        log.info("Start reset password user with normal ldap account: {}", cn);
        if (!ldapProperties.getUrls().startsWith(Const.LDAPS)) {
            throw new ResponseException(BadRequestError.CHANGE_PASSWORD_NOT_SUPPORTED);
        }
        LdapContext ctxGC = initLdapContext(String.format("%s%s", ldapProperties.getFilteredGroup().getPrefix(),
                ldapProperties.getUsername()), ldapProperties.getPassword());
        if (ctxGC == null) {
            throw new ResponseException(BadRequestError.ACCOUNT_LDAP_CAN_NOT_CHANGE_PASSWORD);
        }
        Attribute pass = new BasicAttribute("unicodePwd", encodePassword(request.getNewPassword()));
        ModificationItem item = new ModificationItem(DirContext.REPLACE_ATTRIBUTE, pass);
        try {
            ctxGC.modifyAttributes(getUserDN(cn), new ModificationItem[]{item});
        } catch (NamingException e) {
            log.error("Cant reset password", e);
            throw new ResponseException(BadRequestError.ACCOUNT_LDAP_CAN_NOT_CHANGE_PASSWORD);
        }
        return true;
    }

    @Override
    public List<UserLdapResponse> getAllUserInGroupLdap() {
        log.info("Start get all user in group ldap!");
        LdapContext ctxGC = initLdapContext(String.format("%s%s", ldapProperties.getFilteredGroup().getPrefix(),
                ldapProperties.getUsername()), ldapProperties.getPassword());
        if (ctxGC == null) {
            log.info("Init LDAP context error");
            return new ArrayList<>();
        }
        log.info("Init LDAP context success");

        // filter
        String filterBy = "(&(objectClass=user)(!(objectClass=computer))(sAMAccountName=*))";

        // some attribute should get
        List<String> attributeKeys = Arrays.asList("sAMAccountName", "memberOf",
                "displayName", "userPrincipalName", "mail", "telephoneNumber");

        try {

            // Activate paged results
            int pageSize = Const.MAX_PAGE_SIZE_SYNC_LDAP;
            byte[] cookie = null;
            ctxGC.setRequestControls(new Control[]{new PagedResultsControl(pageSize, Control.NONCRITICAL)});
            int total;
            List<UserLdapResponse> userLdapResponses = new ArrayList<>();

            do {
                /* perform the search */
                SearchControls searchControls = new SearchControls();
                searchControls.setSearchScope(SearchControls.SUBTREE_SCOPE);

                log.info("Begin sync with filter {}, {}", filterBy, ldapProperties.getFilteredGroup().getBaseDnGroup());
                NamingEnumeration<?> objs = ctxGC.search(ldapProperties.getFilteredGroup().getBaseDnGroup(), filterBy, searchControls);
                log.info("Search success with filter {}, {}", filterBy, ldapProperties.getFilteredGroup().getBaseDnGroup());
                while (objs.hasMoreElements()) {

                    SearchResult match = (SearchResult) objs.nextElement();

                    // Get the node's attributes
                    Attributes attrs = match.getAttributes();

                    NamingEnumeration<?> e = attrs.getAll();
                    JSONObject jsonObject = new JSONObject();

                    // Loop through the attributes
                    while (e.hasMoreElements()) {
                        // Get the next attribute
                        Attribute attr = (Attribute) e.nextElement();
                        if (attributeKeys.contains(attr.getID())) {
                            StringBuilder values = new StringBuilder();
                            for (int i = 0; i < attr.size(); i++) {
                                if (i > 0) {
                                    values.append(StringPool.COMMA);
                                }
                                values.append(attr.get(i));
                            }
                            if (Objects.equals(attr.getID(), SAM_ACCOUNT_NAME)) {
                                jsonObject.put(USER_NAME, values.toString());
                            } else {
                                jsonObject.put(attr.getID(), values.toString());
                            }

                        }
                    }
                    try {
                        // read from object json and convert to userLdapResponse
                        UserLdapResponse userLdapResponse = objectMapper.readValue(
                                jsonObject.toJSONString(), UserLdapResponse.class);

                        userLdapResponses.add(userLdapResponse);
                    } catch (JsonProcessingException jsonProcessingException) {
                        log.error("Error when parse userLdapResponse json to object");
                    }
                }
                log.info("Search success, have {} item", userLdapResponses.size());

                // Examine the paged results control response
                Control[] controls = ctxGC.getResponseControls();
                if (controls != null) {
                    for (Control control : controls) {
                        if (control instanceof PagedResultsResponseControl) {
                            PagedResultsResponseControl prrc = (PagedResultsResponseControl) control;
                            total = prrc.getResultSize();
                            if (total != 0) {
                                log.info("***************** END-OF-PAGE " + "(total : " + total + ") *****************\n");
                            } else {
                                log.info("***************** END-OF-PAGE " + "(total: unknown) ***************\n");
                            }
                            cookie = prrc.getCookie();
                        }
                    }
                } else {
                    log.info("No controls were sent from the server");
                }
                // Re-activate paged results
                ctxGC.setRequestControls(new Control[]{new PagedResultsControl(pageSize, cookie, Control.CRITICAL)});

            } while (cookie != null);
            ctxGC.close();

            log.info("Search success, have {} item", userLdapResponses.size());
            return findUserInGroup(userLdapResponses);
        } catch (NamingException | IOException namingException) {
            log.error("Error with namingException " + namingException);
        }
        return new ArrayList<>();
    }

    /**
     * Lọc những user ở trong group cần đồng bộ
     *
     * @param userLdapResponses: list user Ldap
     * @return List UserLdapResponse
     */
    private List<UserLdapResponse> findUserInGroup(List<UserLdapResponse> userLdapResponses) {
        log.debug("Group filter {}", Arrays.asList(ldapProperties.getFilteredGroup().getGroups()));
        return userLdapResponses.stream().filter(userLdapResponse -> {
            // check group of user
            log.debug("Start check user ldap: {}", userLdapResponse.getUserName());
            if (userLdapResponse.getMemberOf() != null) {
                List<String> groups = new ArrayList<>();
                String[] commonGroups = userLdapResponse.getMemberOf().split(StringPool.COMMA);
                for (String groupName : commonGroups) {
                    if (COMMON_NAME.equalsIgnoreCase(groupName.split(StringPool.EQUAL)[0])) {
                        groups.add(groupName.split(StringPool.EQUAL)[1]);
                    }
                }
                log.info("Start check user ldap: {}, member of {}", userLdapResponse.getUserName(), groups);
                for (String group : groups) {
                    boolean isAccountInGroup;
                    isAccountInGroup = Arrays.stream(ldapProperties.getFilteredGroup().getGroups()).anyMatch(g -> g.equalsIgnoreCase(group));
                    if (Boolean.TRUE.equals(isAccountInGroup)) {
                        log.info("Add user ldap: {}", userLdapResponse.getUserName());
                        return true;
                    }
                }
            }
            log.debug("Ignore user ldap: {}", userLdapResponse.getUserName());
            return false;
        }).collect(Collectors.toList());
    }

    private byte[] encodePassword(String password) {
        String quotedPassword = "\"" + password + "\"";
        char[] unicodePwd = quotedPassword.toCharArray();
        byte[] pwdArray = new byte[unicodePwd.length * 2];
        for (int i = 0; i < unicodePwd.length; i++) {
            pwdArray[i * 2 + 1] = (byte) (unicodePwd[i] >>> 8);
            pwdArray[i * 2] = (byte) (unicodePwd[i] & 0xff);
        }
        return pwdArray;
    }

    private String getUserDN(String cnValue) {
        return "CN= " + cnValue + StringPool.COMMA + ldapProperties.getBase();
    }

    private List<String> getJsonElements(NamingEnumeration<?> objs) {
        List<String> jsonElements = new ArrayList<>();

        while (objs.hasMoreElements()) {

            SearchResult match = (SearchResult) objs.nextElement();
            // Get the node's attributes
            Attributes attrs = match.getAttributes();

            NamingEnumeration<?> e = attrs.getAll();
            JSONObject jsonObject = this.getJsonObjectFromAttribute(e);

            // Loop through the attributes

            jsonElements.add(jsonObject.toJSONString());
        }
        return jsonElements;
    }

    private JSONObject getJsonObjectFromAttribute(NamingEnumeration<?> e) {
        // some attribute should get
        List<String> attributeKeys = Arrays.asList("sAMAccountName", "memberOf",
                "displayName", "userPrincipalName", "mail", "telephoneNumber");
        JSONObject jsonObject = new JSONObject();
        while (e.hasMoreElements()) {
            // Get the next attribute
            Attribute attr = (Attribute) e.nextElement();
            if (attributeKeys.contains(attr.getID())) {
                StringBuilder values = new StringBuilder();
                processAttribute(attr, values);
                if (Objects.equals(attr.getID(), SAM_ACCOUNT_NAME)) {
                    jsonObject.put(USER_NAME, values.toString());

                } else {
                    jsonObject.put(attr.getID(), values.toString());

                }
            }
        }
        return jsonObject;
    }

    private void processAttribute(Attribute attr, StringBuilder values) {
        for (int i = 0; i < attr.size(); i++) {
            if (i > 0) values.append(StringPool.COMMA);

            try {
                values.append(attr.get(i));
            } catch (NamingException namingException) {
                log.error("Naming Exception when get value from Attribute {} at element {}", attr, i);
            }
        }
    }

    private List<UserLdapResponse> parseJsonElementToUserLdap(List<String> jsonObjects) {
        List<UserLdapResponse> userLdapResponses = new ArrayList<>();
        jsonObjects.forEach(item -> {
            try {
                userLdapResponses.add(objectMapper.readValue(item, UserLdapResponse.class));
            } catch (JsonProcessingException e) {
                log.error("Error when parse object json");
            }
        });
        return userLdapResponses;
    }
}
