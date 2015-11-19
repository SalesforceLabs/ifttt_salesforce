PoC Salesforce IFTTT Channel
============================

This is a proof-of-concept IFTTT channel for Salesforce.  You can try out the *private* channel by going to:
https://ifttt.com/features/redeem?code=1979-d40cda75ee035941054557c5dec504cd

For some triggers you will need to install a Salesforce managed package:
http://ifttt-salesforce.herokuapp.com/

This app is mostly just a bridge / JSON transformation layer between Salesforce's REST APIs and IFTTT's APIs.  It was built with Play Framework and Scala because those technologies are great at JSON transformation and Reactive / Non-Blocking IO.

Developer Info
--------------

To setup a local development environment do the following:

1. Clone this repo
1. Start a local Redis server
1. Setup ngrok
    1. Install ngrok: https://ngrok.com/
    1. Start ngrok: `ngrok 9000`
1. Create a Salesforce OAuth App
    * OAuth Callback URL: `https://<YOUR ID>.ngrok.com/services/oauth2/authorized`
1. Create a new IFTTT channel for testing: https://developers.ifttt.com/channels
    * `Details > API URL Prefix` = `https://<YOUR ID>.ngrok.com`
    * `Authentication > Scheme` = `My Channel has users with expiring OAuth2 access tokens and uses refresh tokens.`
    * `Authentication > OAuth2 Settings > Client ID` = `<YOUR OAUTH CLIENT ID>`
    * `Authentication > OAuth2 Settings > Client Secret` = `<YOUR OAUTH CLIENT Secret>`
    * `Authentication > OAuth2 Settings > Authorization URL` = `https://<YOUR ID>.ngrok.com/services/oauth2/authorize`
    * `Authentication > OAuth2 Settings > Token URL` = `https://<YOUR ID>.ngrok.com/services/oauth2/token`
1. Install the Salesforce IFTTT Managed Package into a testing org: http://ifttt-salesforce.herokuapp.com/
1. Obtain an Access Token for a user in that testing org
1. Set some env vars for the local Play app:

        export IFTTT_CHANNEL_ID=<YOUR IFTTT CHANNEL SLUG>
        export IFTTT_CHANNEL_KEY=<YOUR IFTTT CHANNEL KEY>
        export IFTTT_TEST_ACCESSTOKEN=<YOUR TESTING SALESFORCE ACCESS TOKEN>

1. Start the Play app: `./activator ~run`
1. Run the IFTTT Channel Activation Test
1. Setup the IFTTT Triggers and Actions
    * TODO
1. Setup sample data on Salesforce for IFTTT tests
    * Create 3 new Opportunities with a `Closed/Won` status
    * Create 3 new `IFTTT Events` objects with the `type` field set to `test`
1. Run the IFTTT Channel Endpoint Test


Salesforce Metadata
-------------------

Fetch the metadata:

    SALESFORCE_USERNAME=foo@bar.com SALESFORCE_PASSWORD=password ./activator force:fetch

Deploy the metadata:

    SALESFORCE_USERNAME=foo@bar.com SALESFORCE_PASSWORD=password ./activator force:deploy