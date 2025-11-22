This is a mermaid diagram of the code base

``` mermaid
graph TD;
      subgraph Use Cases;
        provideUsageRightsOfAssetToCommunityOfChoice([Lease Equipment]);
        stakeTokensOrAssetsAsPartOfTransactionGuarantee([Help People Use Services]);
        verifyComprehension([Verify Knowledge and Understanding of Course]);
        verifiyTaskWithOracle([Verify Task]);
        postCourseAndTestSuiteForExams([Post Test]);
        postTaskForOracleValidation([Post Task]);
        signAnonynomusAribtrationAgreementsWithAribtrator([Sign Aribtration Agreement]);
        postServiceAnnouncement([Post Service Ad]);
        postServiceConsideration([Post Service Bounty for Consideration]);
        postServiceRequest([Post a Request for Service]);
        filterAndFindServiceRequestOrConsideration([Find Work]);
        filterAndFindServiceAnnouncement([Find Contractor]);
      end;

      subgraph NFTs;
        Token --> LIFE_TOKEN;
        NFT --> ASSET_NFT;
        NFT --> KNOWLEDGE_NFT;
        NFT --> TRUST_NFT;
        NFT --> SERVICE_NFT;
      end;


      subgraph Life Token Form;
        value --> Life_Token_Form;
      end;

      subgraph Lease Equipment Form;
        ASSET_NFT --> Asset_Lease_Form;
        LIFE_TOKEN --> Asset_Lease_Form;
      end;

      subgraph Stake Tokens Form;
        Crypto_Address --> Stake_Token_Form;
        LIFE_TOKEN --> Stake_Token_Form;
        ASSET_NFT --> Stake_Token_Form;
        SERVICE_NFT --> Stake_Token_Form;
        stakeExpirationDate --> Stake_Token_Form;
      end;

      subgraph Verify Knowledge and Proficiency Form;
        KNOWLEDGE_NFT --> Verify_Knowledge_Form;
        Scholarship_Crypto_Address --> Verify_Knowledge_Form;
      end;


      subgraph Verify Task Form;
        SERVICE_NFT --> Verify_Task_Form;
        Contractor_Crypto_Address --> Verify_Task_Form;
      end;


      subgraph Post Test Form;
        test_title[Title] --> Post_Test_Form;
        test_summary[Summary] --> Post_Test_Form;
        test_description[Description] --> Post_Test_Form;
        test_images[Images] --> Post_Test_Form;
        test_question[Question] --> test_question_answer_set;
        test_answer[Answer] --> test_question_answer_set;
        test_question_answer_set[Test Question Answer Set] --> test_question_exam;
        test_question_exam[Test Exam] --> Post_Test_Form;
      end;


      subgraph Post Task Form;
        task_title[Title] --> Post_Task_Form;
        task_summary[Summary] --> Post_Task_Form;
        task_description[Description] --> Post_Task_Form;
        task_images[Images] --> Post_Task_Form;
        task_notes[Notes] --> Post_Task_Form;
        KNOWLEDGE_NFT --> Post_Task_Form;
        KNOWLEDGE_NFT --> Post_Task_Form;
      end;


      subgraph Sign Aribtration Agreement Form;
        Aribitrator_Crypto_Address --> Sign_Aribtration_Agreement_Form;
        service_agreement_summary[Summary] --> Sign_Aribtration_Agreement_Form;
        service_agreement_description[Description`] --> Sign_Aribtration_Agreement_Form;
        service_agreement_images[Images] --> Sign_Aribtration_Agreement_Form;
        service_agreement_notes[Notes] --> Sign_Aribtration_Agreement_Form;
        service_agreement_keywords[Keywords] --> Sign_Aribtration_Agreement_Form;
      end;


      subgraph Post Service Announcement Form;
        service_announcement_title[Title] --> Post_Service_Announcement_Form;
        service_announcement_summary[Summary]  --> Post_Service_Announcement_Form;
        service_announcement_description[Description]  --> Post_Service_Announcement_Form;
        service_announcement_images[Images] --> Post_Service_Announcement_Form;
        service_announcement_notes[Notes]  --> Post_Service_Announcement_Form;
        service_announcement_expiration_date[Notes]  --> Post_Service_Announcement_Form;
        KNOWLEDGE_NFT  --> Post_Service_Announcement_Form;
        LIFE_TOKEN --> Post_Service_Announcement_Form;
      end;


      subgraph Post Service Request Form;
        service_request_title[Title] --> Post_Service_Request_Form;
        service_request_summary[Summary] --> Post_Service_Request_Form;
        service_request_description[Description] --> Post_Service_Request_Form;
        service_request_images[Images] --> Post_Service_Request_Form;
        service_request_notes[Notes] --> Post_Service_Request_Form;
        KNOWLEDGE_NFT --> Post_Service_Request_Form;
      end;


      subgraph Post Service Consideration Form;
        service_consideration_title[Title] --> Post_Service_Consideration_Form;
        service_consideration_summary[Summary] --> Post_Service_Consideration_Form;
        service_consideration_description[Description] --> Post_Service_Consideration_Form;
        service_consideration_images[Images] --> Post_Service_Consideration_Form;
        service_consideration_notes[Notes] --> Post_Service_Consideration_Form;
        KNOWLEDGE_NFT --> Post_Service_Consideration_Form;
        LIFE_TOKEN --> Post_Service_Consideration_Form;
      end;


      subgraph Request Service Form;
        request_service_title[Title] --> Post_Request_Form;
        request_service_summary[Summary]  --> Post_Request_Form;
        request_service_description[Description]  --> Post_Request_Form;
        request_service_images[Images] --> Post_Request_Form;
        request_service_notes[Notes]  --> Post_Request_Form;
        KNOWLEDGE_NFT  --> Post_Request_Form;
        LIFE_TOKEN --> Post_Request_Form;
      end;


      subgraph Find Work Form;
        search_date_time_range[Time range] --> Service_Work_Filter_Form;
        search_coords[Location] --> Service_Work_Filter_Form;
        search_radius[Search Radius] --> Service_Work_Filter_Form;
        search_keywords[Tags] --> Service_Work_Filter_Form;
        KNOWLEDGE_NFT  --> Service_Work_Filter_Form;
        LIFE_TOKEN --> Service_Work_Filter_Form;
    
      end;
```