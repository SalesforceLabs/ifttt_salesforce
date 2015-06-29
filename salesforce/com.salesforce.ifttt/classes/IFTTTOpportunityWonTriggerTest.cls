@isTest
public class IFTTTOpportunityWonTriggerTest implements HttpCalloutMock {

    private static HttpRequest request;
    private static HttpResponse response;

    public HTTPResponse respond(HTTPRequest req) {
        request = req;
        response = new HttpResponse();
        response.setStatusCode(200);
        return response;
    }
    
    @isTest static void testInsertTrigger() {
        Test.setMock(HttpCalloutMock.class, new IFTTTOpportunityWonTriggerTest());
        
        Opportunity opp = new Opportunity(Name = 'Test', CloseDate = Date.Today(), StageName = 'Closed Won');
        insert opp;
        
        List<IFTTT_Event__c> events = [SELECT Id FROM IFTTT_Event__c WHERE Related_Object_Id__c = :opp.Id];

        System.assertEquals(1, events.size());
    }
    
    @isTest static void testUpdateTrigger() {        
        Test.setMock(HttpCalloutMock.class, new IFTTTOpportunityWonTriggerTest());
        
        Opportunity opp = new Opportunity(Name = 'Test', CloseDate = Date.Today(), StageName = 'Prospecting');
        insert opp;
        opp.StageName = 'Closed Won';
        update opp;

        List<IFTTT_Event__c> events = [SELECT Id FROM IFTTT_Event__c WHERE Related_Object_Id__c = :opp.Id];

        System.assertEquals(1, events.size());
    }
}