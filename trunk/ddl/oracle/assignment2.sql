
    create table A2_ASSIGNMENT_T (
        ASSIGNMENT_ID number(19,0) not null,
        VERSION number(10,0) not null,
        GRADEBOOK_ITEM_ID number(19,0),
        CONTEXT varchar2(99) not null,
        TITLE varchar2(255) not null,
        DRAFT number(1,0),
        SORT_INDEX number(10,0) not null,
        OPEN_DATE date not null,
        ACCEPT_UNTIL_DATE date,
        GRADED number(1,0),
        DUE_DATE date,
        HONOR_PLEDGE number(1,0),
        INSTRUCTIONS clob,
        REQUIRES_SUBMISSION number(1,0),
        SUBMISSION_TYPE number(10,0) not null,
        SEND_SUBMISSION_NOTIF number(1,0),
        HAS_ANNOUNCEMENT number(1,0),
        ANNOUNCEMENT_ID varchar2(99),
        ADDED_TO_SCHEDULE number(1,0),
        EVENT_ID varchar2(99),
        NUM_SUB_ALLOWED number(10,0) not null,
        CONTENT_REVIEW_REF varchar2(255),
        CREATOR varchar2(99) not null,
        CREATE_DATE date not null,
        MODIFIED_BY varchar2(99),
        MODIFIED_DATE date,
        REMOVED number(1,0),
        primary key (ASSIGNMENT_ID)
    );

    create table A2_ASSIGN_ATTACH_T (
        ASSIGN_ATTACH_ID number(19,0) not null,
        VERSION number(10,0) not null,
        ASSIGNMENT_ID number(19,0) not null,
        ATTACHMENT_REFERENCE varchar2(255) not null,
        primary key (ASSIGN_ATTACH_ID),
        unique (ASSIGNMENT_ID, ATTACHMENT_REFERENCE)
    );

    create table A2_ASSIGN_GROUP_T (
        ASSIGNMENT_GROUP_ID number(19,0) not null,
        VERSION number(10,0) not null,
        ASSIGNMENT_ID number(19,0) not null,
        GROUP_REF varchar2(255) not null,
        primary key (ASSIGNMENT_GROUP_ID),
        unique (ASSIGNMENT_ID, GROUP_REF)
    );

    create table A2_SUBMISSION_ATTACH_T (
        SUBMISSION_ATTACH_ID number(19,0) not null,
        SUB_ATTACH_TYPE varchar2(1) not null,
        VERSION number(10,0) not null,
        SUBMISSION_VERSION_ID number(19,0) not null,
        ATTACHMENT_REFERENCE varchar2(255) not null,
        primary key (SUBMISSION_ATTACH_ID),
        unique (SUBMISSION_VERSION_ID, ATTACHMENT_REFERENCE)
    );

    create table A2_SUBMISSION_T (
        SUBMISSION_ID number(19,0) not null,
        VERSION number(10,0) not null,
        ASSIGNMENT_ID number(19,0) not null,
        USER_ID varchar2(99) not null,
        RESUBMIT_CLOSE_DATE date,
        NUM_SUB_ALLOWED number(10,0),
        COMPLETED number(1,0),
        CREATED_BY varchar2(99) not null,
        CREATED_DATE date not null,
        MODIFIED_BY varchar2(99),
        MODIFIED_DATE date,
        primary key (SUBMISSION_ID),
        unique (ASSIGNMENT_ID, USER_ID)
    );

    create table A2_SUBMISSION_VERSION_T (
        SUBMISSION_VERSION_ID number(19,0) not null,
        VERSION number(10,0) not null,
        SUBMISSION_ID number(19,0) not null,
        SUBMITTED_DATE date,
        SUBMITTED_VERSION_NUMBER number(10,0) not null,
        FEEDBACK_RELEASED_DATE date,
        SUBMITTED_TEXT clob,
        ANNOTATED_TEXT clob,
        FEEDBACK_NOTES clob,
        DRAFT number(1,0),
        CREATED_BY varchar2(99) not null,
        CREATED_DATE date not null,
        MODIFIED_BY varchar2(99),
        MODIFIED_DATE date,
        LAST_FEEDBACK_BY varchar2(99),
        LAST_FEEDBACK_DATE date,
        STUDENT_SAVE_DATE date,
        FEEDBACK_LAST_VIEWED date,
        primary key (SUBMISSION_VERSION_ID),
        unique (SUBMISSION_ID, SUBMITTED_VERSION_NUMBER)
    );

    create index A2_ASSIGN_REMOVED_I on A2_ASSIGNMENT_T (REMOVED);

    create index A2_ASSIGN_CONTEXT_I on A2_ASSIGNMENT_T (CONTEXT);

    create index A2_ASSIGN_ATTACH_ASSIGN_I on A2_ASSIGN_ATTACH_T (ASSIGNMENT_ID);

    alter table A2_ASSIGN_ATTACH_T 
        add constraint FKFF1065FC175E3454 
        foreign key (ASSIGNMENT_ID) 
        references A2_ASSIGNMENT_T;

    create index A2_ASSIGN_GROUP_ASSIGN_I on A2_ASSIGN_GROUP_T (ASSIGNMENT_ID);

    alter table A2_ASSIGN_GROUP_T 
        add constraint FK39B6CD12175E3454 
        foreign key (ASSIGNMENT_ID) 
        references A2_ASSIGNMENT_T;

    create index SUB_ATTACH_TYPE_I on A2_SUBMISSION_ATTACH_T (SUB_ATTACH_TYPE);

    create index A2_SUB_ATTACH_VERSION_I on A2_SUBMISSION_ATTACH_T (SUBMISSION_VERSION_ID);

    alter table A2_SUBMISSION_ATTACH_T 
        add constraint FK3FF3D33F49CF92D6 
        foreign key (SUBMISSION_VERSION_ID) 
        references A2_SUBMISSION_VERSION_T;

    create index A2_SUBMISSION_USER_ID_I on A2_SUBMISSION_T (USER_ID);

    create index A2_SUBMISSION_ASSIGN_I on A2_SUBMISSION_T (ASSIGNMENT_ID);

    alter table A2_SUBMISSION_T 
        add constraint FK4A5A558F175E3454 
        foreign key (ASSIGNMENT_ID) 
        references A2_ASSIGNMENT_T;

    create index A2_SUB_VERSION_SUBMITTED_NUM on A2_SUBMISSION_VERSION_T (SUBMITTED_VERSION_NUMBER);

    create index A2_SUB_VERSION_SUB_DATE_I on A2_SUBMISSION_VERSION_T (SUBMITTED_DATE);

    create index A2_SUB_VERSION_SUB_I on A2_SUBMISSION_VERSION_T (SUBMISSION_ID);

    alter table A2_SUBMISSION_VERSION_T 
        add constraint FK873450C88ABCB1A5 
        foreign key (SUBMISSION_ID) 
        references A2_SUBMISSION_T;

    create sequence A2_ASSIGNMENT_S;

    create sequence A2_ASSIGN_ATTACH_S;

    create sequence A2_ASSIGN_GROUP_S;

    create sequence A2_SUBMISSION_ATTACH_S;

    create sequence A2_SUBMISSION_S;

    create sequence A2_SUBMISSION_VERSION_S;
