public class HttpCalloutMockImpl implements HttpCalloutMock {

    public HttpRequest request;
    public HttpResponse response;

    public HTTPResponse respond(HTTPRequest req) {
        request = req;
        response = new HttpResponse();
        response.setStatusCode(200);
        return response;
    }

}