@isTest
public class IFTTTOpportunityWonTriggerTest {

    @isTest static void testInsertTrigger() {
        Opportunity opp = new Opportunity(Name = 'Test', CloseDate = Date.Today(), StageName = 'Closed Won');
        insert opp;
        
        List<IFTTT_Event__c> events = [SELECT Id FROM IFTTT_Event__c WHERE Related_Object_Id__c = :opp.Id];

        System.assertEquals(1, events.size());
    }

    @isTest static void testUpdateTrigger() {
        Opportunity opp = new Opportunity(Name = 'Test', CloseDate = Date.Today(), StageName = 'Prospecting');
        insert opp;
        opp.StageName = 'Closed Won';
        update opp;

        List<IFTTT_Event__c> events = [SELECT Id FROM IFTTT_Event__c WHERE Related_Object_Id__c = :opp.Id];

        System.assertEquals(1, events.size());
    }

}