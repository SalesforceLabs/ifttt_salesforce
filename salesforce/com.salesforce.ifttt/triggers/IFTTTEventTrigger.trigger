trigger IFTTTEventTrigger on ifttt__IFTTT_Event__c (after insert, after update) {

    String url = 'https://ifttt-salesforce.herokuapp.com/ifttt/v1/webhook/ifttt_event';

    String content = Webhook.jsonContent(Trigger.new, Trigger.old);

    Webhook.callout(url, content);
    
}