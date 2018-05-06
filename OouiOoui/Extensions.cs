using System;
using System.Runtime.CompilerServices;
using System.Security.Cryptography;
using System.Text;
using System.Threading.Tasks;

namespace OouiOoui
{
  public static class Extensions
  {
    [ThreadStatic]
    static SHA256 sha256;

    public static string Hash(this byte[] bytes)
    {
      var sha = sha256;
      if (sha == null) 
      {
          sha = SHA256.Create();
          sha256 = sha;
      }
      var data = sha.ComputeHash(bytes);
      StringBuilder sb = new StringBuilder (data.Length*2);
      foreach (var b in data)
      {
        sb.Append (b.ToString("x2"));
      }
      return sb.ToString();
    }

    public static ConfiguredTaskAwaitable<T> NoSynchronizationContext<T>(this Task<T> t)
    {
      return t.ConfigureAwait(false);
    }

    public static void TraceInfo(this string msg)
    {
      Console.WriteLine($"INFO : {msg}");
    }

    public static void TraceError(this string msg)
    {
      Console.WriteLine($"ERROR: {msg}");
    }

  }
}
