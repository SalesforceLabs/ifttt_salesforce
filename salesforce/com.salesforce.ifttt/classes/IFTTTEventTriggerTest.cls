@isTest
public class IFTTTEventTriggerTest {

    @isTest static void testTrigger() {
        HttpCalloutMockImpl httpMock = new HttpCalloutMockImpl();
        Test.setMock(HttpCalloutMock.class, httpMock);

        IFTTT_Event__c o = new IFTTT_Event__c();

        Test.startTest();
        insert o;
        update o;
        delete o;
        Test.stopTest();

        System.assertEquals(200, httpMock.response.getStatusCode());
        System.assertEquals('https://ifttt-salesforce.herokuapp.com/ifttt/v1/webhook/ifttt_event', httpMock.request.getEndpoint());

        if (httpMock.request != null) {
            Map<String, Object> jsonRequest = (Map<String, Object>) JSON.deserializeUntyped(httpMock.request.getBody());
            System.assertNotEquals(null, jsonRequest.get('userId'));
        }
    }

}