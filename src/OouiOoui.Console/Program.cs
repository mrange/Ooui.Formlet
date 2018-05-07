namespace OouiOoui.Console
{
  using System;

  class Program
  {
    static void Main(string[] args)
    {
      using(var listener = new OouiOoui.Server.Listener())
      { 
        var startTask = listener.Start();

        Console.WriteLine("Press any key");

        Console.ReadLine();
      }
    }
  }
}
