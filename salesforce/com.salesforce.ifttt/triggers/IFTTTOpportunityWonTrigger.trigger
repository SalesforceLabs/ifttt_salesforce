trigger IFTTTOpportunityWonTrigger on Opportunity (after insert, after update) {

    for (Opportunity newOpp : Trigger.new) {
        Opportunity oldOpp = null;
        if (Trigger.oldMap != null) {
    		oldOpp = Trigger.oldMap.get(newOpp.ID);
        }
		Boolean oldIsNotClosed = (oldOpp == null) || (oldOpp.StageName != 'Closed Won');
        if ((oldIsNotClosed) && (newOpp.StageName == 'Closed Won')) {
        	IFTTT_Event__c e = new IFTTT_Event__c(Name = 'Opportunity Was Won');
        	e.Type__c = 'Opportunity Won';
            e.Related_Object_Type__c = 'Opportunity';
            e.Related_Object_Id__c = newOpp.ID;
            insert e;
        }
    }
    
}