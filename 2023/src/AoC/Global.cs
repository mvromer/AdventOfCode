namespace AoC;

public static class Global
{
    public static void InvokeVariant(string[] args, Action<string> partA, Action<string> partB)
    {
        var variant = args[0];
        var useExample = args.Length > 1 && args[1] == "ex";

        switch (variant)
        {
            case "a":
                InvokeVariant(variant, useExample, partA);
                break;

            case "b":
                InvokeVariant(variant, useExample, partB);
                break;

            default:
                throw new ArgumentException("Invalid variant");
        }
    }

    private static void InvokeVariant(string variant, bool useExample, Action<string> part)
    {
        var inputFileName = useExample ? $"ex{variant}.txt" : "input.txt";
        part(inputFileName);
    }
}
