﻿using System.Net;
using System.Text;
using System.Threading.Tasks;

namespace OouiOoui.Server
{
  sealed class Session
  {
    readonly HttpListenerContext context;

    public Session(HttpListenerContext httpListenerContext)
    {
      context = httpListenerContext;
    }

    public readonly static byte[] Script = Encoding.UTF8.GetBytes("hello");
    public readonly static string ScriptEtag = $"\"{Script.Hash()}\"";
    
    public async Task<Unit> Start()
    {
      if (context.Request.IsWebSocketRequest)
      {
        var webSocket = await context.AcceptWebSocketAsync("OouiOoui").NoSynchronizationContext();
      }
      else
      {
        var response = context.Response;
        response.StatusCode       = 200;
        response.ContentType      = "application/javascript";
        response.ContentEncoding  = Encoding.UTF8;
        response.ContentLength64  = Script.LongLength;
        response.AddHeader("Cache-Control", "public, max-age=60");
        response.AddHeader("Etag"         , ScriptEtag);
        using (var stream = context.Response.OutputStream) {
          stream.Write(Script, 0, Script.Length);
        }
        response.Close();
      }
      return Unit.Value;
    }
  }
}
