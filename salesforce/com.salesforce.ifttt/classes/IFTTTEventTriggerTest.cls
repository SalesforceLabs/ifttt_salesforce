@isTest
public class IFTTTEventTriggerTest implements HttpCalloutMock {
    private static HttpRequest request;
    private static HttpResponse response;

    public HTTPResponse respond(HTTPRequest req) {
        request = req;
        response = new HttpResponse();
        response.setStatusCode(200);
        return response;
    }
    
    @isTest static void testTrigger() {
        Test.setMock(HttpCalloutMock.class, new IFTTTEventTriggerTest());

        IFTTT_Event__c o = new IFTTT_Event__c();

        Test.startTest();
        insert o;
        update o;
        delete o;
        Test.stopTest();

        System.assertEquals(200, response.getStatusCode());
        System.assertEquals('https://ifttt-salesforce.herokuapp.com/ifttt/v1/webhook/ifttt_event', request.getEndpoint());

        if (request != null) {
            Map<String, Object> jsonRequest = (Map<String, Object>) JSON.deserializeUntyped(request.getBody());
            System.assertNotEquals(null, jsonRequest.get('userId'));
        }
    }

}