using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;

namespace Aoc2020.CSharp.Day07
{
    public static class PuzzleB
    {
        private record Bag(string Color, IEnumerable<ContainsRelationship> Contents);

        private record ContainsRelationship(int Count, string Color);

        private class LuggageGraph
        {
            private Dictionary<string, HashSet<ContainsRelationship>> Contents { get; } =
                new Dictionary<string, HashSet<ContainsRelationship>>();

            public void AddBag(string color)
            {
                if (this.Contents.ContainsKey(color))
                    return;

                this.Contents[color] = new HashSet<ContainsRelationship>();
            }

            public void AddContainerBag(string containerColor, int containedCount, string containedColor)
            {
                AddBag(containerColor);
                AddBag(containedColor);
                this.Contents[containerColor].Add(new ContainsRelationship(containedCount, containedColor));
            }

            public IEnumerable<Bag> GetBags()
            {
                foreach ((var color, var contents) in this.Contents)
                {
                    yield return new Bag(color, contents);
                }
            }

            public int CountContainedBags(string sourceColor)
            {
                // Do an exhaustive breadth first search from the source bag to count all the bags
                // contained by it. By exhaustive, I mean we don't track bags we've "visited" but
                // but instead traverse over previously seen nodes so long as we're arriving from a
                // different path.
                var remainingColorsWithCountScale = new Queue<(string, int)>();
                int numberContained = 0;

                remainingColorsWithCountScale.Enqueue((sourceColor, 1));
                while (remainingColorsWithCountScale.Count > 0)
                {
                    (var currentColor, var currentScale) = remainingColorsWithCountScale.Dequeue();
                    foreach ((var containedCount, var containedColor) in this.Contents[currentColor])
                    {
                        int scaledContainedCount = currentScale * containedCount;
                        numberContained += scaledContainedCount;
                        remainingColorsWithCountScale.Enqueue((containedColor, scaledContainedCount));
                    }
                }

                return numberContained;
            }
        }

        private static LuggageGraph ParseRules(string inputFileName)
        {
            var luggageGraph = new LuggageGraph();

            foreach (var line in File.ReadLines(inputFileName))
            {
                var pattern = (
                    @"^(?<containerColor>\w+ \w+) bags contain " +
                    @"(?:no other bags|" +
                    @"(?:(?<containedCount>\d+) (?<containedColor>\w+ \w+) bags?)" +
                    @"(?:, (?<containedCount>\d+) (?<containedColor>\w+ \w+) bags?)*)\.$"
                );

                var match = Regex.Match(line, pattern);

                if (match.Success)
                {
                    var containerColor = match.Groups["containerColor"].Value;
                    var containedCounts = match.Groups["containedCount"].Captures.Select(c => Int32.Parse(c.Value));
                    var containedColors = match.Groups["containedColor"].Captures.Select(c => c.Value);

                    // For this puzzle we always want to add the container bag to the graph, even if
                    // it contains no other bags.
                    luggageGraph.AddBag(containerColor);

                    containedCounts
                        .Zip(containedColors)
                        .ToList()
                        .ForEach(((int count, string color) contained) =>
                            luggageGraph.AddContainerBag(containerColor, contained.count, contained.color));
                }
            }

            return luggageGraph;
        }

        public static void Main(string inputFileName)
        {
            var luggageGraph = ParseRules(inputFileName);
            const string sourceColor = "shiny gold";
            var numberContained = luggageGraph.CountContainedBags(sourceColor);
            Console.WriteLine($"Number of bags that are eventually contained by a {sourceColor} bag is {numberContained}");
        }
    }
}
