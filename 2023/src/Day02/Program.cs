using System.Text.RegularExpressions;
using static AoC.Global;
using static Constants;

InvokeVariant(args, PartA, PartB);

static void PartA(string inputFileName)
{
    const int MaxRed = 12;
    const int MaxGreen = 13;
    const int MaxBlue = 14;

    int idSum = 0;

    foreach (var line in File.ReadLines(inputFileName))
    {
        var gameMatch = GamePattern().Match(line);
        if (gameMatch.Success)
        {
            var drawnColors = gameMatch
                .Groups["Draws"]
                .Value
                .Split(';')
                .SelectMany(draw => draw.Split(','));

            bool isPossible = true;
            foreach (var drawnColor in drawnColors)
            {
                var drawnColorMatch = DrawnColorPattern().Match(drawnColor);
                if (drawnColorMatch.Success)
                {
                    var colorCount = int.Parse(drawnColorMatch.Groups["Count"].ValueSpan);
                    var color = drawnColorMatch.Groups["Color"].Value;
                    isPossible &=
                        (color == "red" && colorCount <= MaxRed)
                        || (color == "green" && colorCount <= MaxGreen)
                        || (color == "blue" && colorCount <= MaxBlue);
                }

                if (!isPossible)
                    break;
            }

            if (isPossible)
                idSum += int.Parse(gameMatch.Groups["GameId"].ValueSpan);
        }
    }

    Console.WriteLine(idSum);
}

static void PartB(string inputFileName)
{
    int powerSum = 0;

    foreach (var line in File.ReadLines(inputFileName))
    {
        var gameMatch = GamePattern().Match(line);
        if (gameMatch.Success)
        {
            var drawnColors = gameMatch
                .Groups["Draws"]
                .Value
                .Split(';')
                .SelectMany(draw => draw.Split(','));

            var minRed = 0;
            var minGreen = 0;
            var minBlue = 0;

            foreach (var drawnColor in drawnColors)
            {
                var drawnColorMatch = DrawnColorPattern().Match(drawnColor);
                if (drawnColorMatch.Success)
                {
                    var colorCount = int.Parse(drawnColorMatch.Groups["Count"].ValueSpan);
                    var color = drawnColorMatch.Groups["Color"].Value;
                    switch (color)
                    {
                        case "red":
                            minRed = Math.Max(minRed, colorCount);
                            break;

                        case "green":
                            minGreen = Math.Max(minGreen, colorCount);
                            break;

                        case "blue":
                            minBlue = Math.Max(minBlue, colorCount);
                            break;
                    }
                }
            }

            powerSum += minRed * minGreen * minBlue;
        }
    }

    Console.WriteLine(powerSum);
}

static partial class Constants
{
    [GeneratedRegex(@"^Game (?<GameId>\d+): (?<Draws>.*)$")]
    public static partial Regex GamePattern();

    [GeneratedRegex(@"(?<Count>\d+) (?<Color>blue|green|red)")]
    public static partial Regex DrawnColorPattern();
}
