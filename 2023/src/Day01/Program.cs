using static AoC.Global;
using static Constants;

InvokePart(args, PartA, PartB);

static void PartA(string inputFileName)
{
    int sum = 0;
    foreach (var line in File.ReadLines(inputFileName))
    {
        var iFirstDigit = line.IndexOfAny(Digits);
        var iLastDigit = line.LastIndexOfAny(Digits);
        sum += 10 * int.Parse(line.AsSpan(iFirstDigit, 1)) + int.Parse(line.AsSpan(iLastDigit, 1));
    }

    Console.WriteLine(sum);
}

static void PartB(string inputFileName)
{
    int sum = 0;
    foreach (var line in File.ReadLines(inputFileName))
    {
        var firstOccurrences = DigitWords
            .Select((word, iWord) => (OccurrenceIndex: line.IndexOf(word), Value: iWord + 1))
            .Concat(
                Digits.Select(
                    (digit, iDigit) => (OccurrenceIndex: line.IndexOf(digit), Value: iDigit)
                )
            )
            .Where(x => x.OccurrenceIndex >= 0);

        var lastOccurrences = DigitWords
            .Select((word, iWord) => (OccurrenceIndex: line.LastIndexOf(word), Value: iWord + 1))
            .Concat(
                Digits.Select(
                    (digit, iDigit) => (OccurrenceIndex: line.LastIndexOf(digit), Value: iDigit)
                )
            )
            .Where(x => x.OccurrenceIndex >= 0);

        sum += 10 * firstOccurrences.MinBy(x => x.OccurrenceIndex).Value;
        sum += lastOccurrences.MaxBy(x => x.OccurrenceIndex).Value;
    }

    Console.WriteLine(sum);
}

static class Constants
{
    public static readonly char[] Digits = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
    public static readonly string[] DigitWords =
    [
        "one",
        "two",
        "three",
        "four",
        "five",
        "six",
        "seven",
        "eight",
        "nine"
    ];
}
