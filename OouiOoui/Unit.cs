namespace OouiOoui
{
  public sealed class Unit
  {
    public static readonly Unit Value = new Unit();

    public override bool Equals(object obj)
    {
      return obj is Unit;
    }

    public override int GetHashCode()
    {
      return 0x55555555;
    }

    public override string ToString()
    {
      return "Unit";
    }
  }
}
