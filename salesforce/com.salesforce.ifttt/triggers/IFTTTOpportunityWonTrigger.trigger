trigger IFTTTOpportunityWonTrigger on Opportunity (after insert, after update) {

    // set the http mock when running in a test
    if (Test.isRunningTest()) {
        Test.setMock(HttpCalloutMock.class, new HttpCalloutMockImpl());
    }

    String url = 'http://ifttt-salesforce.herokuapp.com/ifttt/v1/webhook/opportunity_was_won';

    String content = Webhook.jsonContent(Trigger.new, Trigger.old);

    Webhook.callout(url, content);

    List<IFTTT_Event__c> newIftttEvents = new List<IFTTT_Event__c>();

    for (Opportunity newOpp : Trigger.new) {
        Opportunity oldOpp = null;
        if (Trigger.oldMap != null) {
            oldOpp = Trigger.oldMap.get(newOpp.ID);
        }

        Boolean oldIsNotClosed = (oldOpp == null) || (oldOpp.StageName != 'Closed Won');
        if ((oldIsNotClosed) && (newOpp.StageName == 'Closed Won')) {
            IFTTT_Event__c e = new IFTTT_Event__c(Name = newOpp.Name);
            e.Type__c = 'Opportunity Won';
            e.Related_Object_Type__c = 'Opportunity';
            e.Related_Object_Id__c = newOpp.ID;
            newIftttEvents.add(e);
        }
    }

    insert newIftttEvents;

}