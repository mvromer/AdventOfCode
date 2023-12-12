namespace AoC;

public static class Global
{
    public static void InvokePart(string[] args, Action<string> partA, Action<string> partB)
    {
        var part = args[0];
        var useExample = args.Length > 1 && args[1] == "ex";

        switch (part)
        {
            case "a":
                InvokePart(part, useExample, partA);
                break;

            case "b":
                InvokePart(part, useExample, partB);
                break;

            default:
                throw new ArgumentException("Invalid variant");
        }
    }

    private static void InvokePart(string part, bool useExample, Action<string> partAction)
    {
        var inputFileName = useExample ? $"ex{part}.txt" : "input.txt";
        partAction(inputFileName);
    }
}
