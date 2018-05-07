using System;
using System.Net;
using System.Threading;
using System.Threading.Tasks;

namespace OouiOoui.Server
{

  public sealed class Listener : IDisposable
  {
    readonly HttpListener listener;

    public Listener()
    {
      listener = new HttpListener ();
      listener.Prefixes.Add("http://localhost:8080/");
    }

    public async Task<Unit> Start()
    {
      listener.Start();
      return await WaitForContext();
    }

    async Task<Unit> WaitForContext()
    {
      "Waiting for connection".TraceInfo();
      $"{Environment.StackTrace}".TraceInfo();
      
      var httpListenerContext = await listener.GetContextAsync().NoSynchronizationContext();

      var session = new Session(httpListenerContext);
      await session.Start().NoSynchronizationContext();

      return await WaitForContext().NoSynchronizationContext();
    }

    public void Dispose()
    {
      listener.Stop();
    }
  }
}
