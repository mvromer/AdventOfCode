using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;

// Day 7 is implemented in C# because it is inherently a graph problem, and from what I can tell,
// most approaches for implementing graph data structures and algorithms in a functional programming
// language tend to mirror imperative implementations. This typically leads to some very complicated
// and inelegant functional programming code that also fails to achieve the same performance as the
// imperative implementations of classical graph algorithms.
//
// The most promising resource I've found on how to represent and work with graphs in functional
// programming is Martin Erwig's work on inductive graph representations. Based on a cursory read of
// his paper, it seems like his approach has the dual benefit of both matching the performance of
// imperative implementations and producing idiomatic functional programming code. He used his work
// to author the Functional Graph Library (FGL) for Haskell. His original paper and the FGL user
// guide are available here:
//
// Original paper - http://web.engr.oregonstate.edu/~erwig/papers/InductiveGraphs_JFP01.pdf
// FGL User Guide - http://web.engr.oregonstate.edu/~erwig/fgl/haskell/old/fgl0103.pdf
//
// There appears to be a NuGet package named Hekate that implements the ideas in the above links and
// exposes an API for use in F#. The downside is that the documentation for it is almost nonexistent
// and would require more time to figure out than would take to simply implement Day 7's puzzles
// imperatively in C#.

namespace Aoc2020.CSharp.Day07
{
    public static class PuzzleA
    {
        private record Bag(string Color, IEnumerable<ContainedByRelationship> Containers);

        private record ContainedByRelationship(int Count, string Color);

        private class LuggageGraph
        {
            private Dictionary<string, HashSet<ContainedByRelationship>> ContainerBags { get; } =
                new Dictionary<string, HashSet<ContainedByRelationship>>();

            public void AddBag(string color)
            {
                if (this.ContainerBags.ContainsKey(color))
                    return;

                this.ContainerBags[color] = new HashSet<ContainedByRelationship>();
            }

            public void AddContainerBag(string containerColor, int containedCount, string containedColor)
            {
                AddBag(containerColor);
                AddBag(containedColor);
                this.ContainerBags[containedColor].Add(new ContainedByRelationship(containedCount, containerColor));
            }

            public IEnumerable<Bag> GetBags()
            {
                foreach ((var color, var containers) in this.ContainerBags)
                {
                    yield return new Bag(color, containers);
                }
            }

            public IEnumerable<string> FindAllContainerColors(string targetColor)
            {
                // Do a breadth first search from the target bag to find all container bags that
                // are reachable from it.
                var remainingColors = new Queue<string>();
                var visitedColors = new HashSet<string>();

                remainingColors.Enqueue(targetColor);
                while (remainingColors.Count > 0)
                {
                    var currentColor = remainingColors.Dequeue();
                    foreach ((_, var containerColor) in this.ContainerBags[currentColor])
                    {
                        if (!visitedColors.Contains(containerColor))
                        {
                            yield return containerColor;
                            visitedColors.Add(containerColor);
                            remainingColors.Enqueue(containerColor);
                        }
                    }
                }
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
            const string targetColor = "shiny gold";
            var numberContainers = luggageGraph.FindAllContainerColors(targetColor).Count();
            Console.WriteLine($"Number of bags that can eventually contain a {targetColor} bag is {numberContainers}");
        }
    }
}
