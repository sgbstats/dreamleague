export default {
  async fetch(request, env, ctx) {
    const targetHost = "sgbstats-dreamleague.share.connect.posit.cloud";
    const url = new URL(request.url);

    // Swap the hostname to Posit Cloud
    url.hostname = targetHost;
    url.protocol = "https:";

    // Create a new request to Posit Cloud while preserving headers/body
    const modifiedRequest = new Request(url.toString(), {
      method: request.method,
      headers: request.headers,
      body: request.body,
      redirect: "follow",
    });

    // Forward host header so Posit Cloud knows how to respond
    modifiedRequest.headers.set("Host", targetHost);

    // Fetch the response from Posit Cloud
    const response = await fetch(modifiedRequest);

    // Return the response directly back to the user
    return response;
  },
};