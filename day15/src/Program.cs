using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;

namespace ConsoleApp1
{
	class Program
	{
		private static readonly List<int> Spoken = new List<int> { 13, 16, 0, 12, 15, 1 };
		private static int Target = 30000000;

		static void Main(string[] args)
		{
			var cache = new Dictionary<int, int>();
			for (var j = 0; j < Spoken.Count - 1; j++)
			{
				cache[Spoken[j]] = j + 1;
			}
			var i = Spoken.Count;
			var lastSpoken = Spoken.Last();

			var sw = Stopwatch.StartNew();
			while (i < Target)
			{
				lastSpoken = Turn(i++, lastSpoken, cache);
			}
			sw.Stop();

			Console.WriteLine(lastSpoken);
			Console.WriteLine("Took {0} secs", sw.Elapsed.TotalSeconds);
		}

		private static int Turn(int index, int lastSpoken, IDictionary<int, int> dict)
		{
			var spoken = 0;
			if (dict.TryGetValue(lastSpoken, out var prevIndex))
			{
				spoken = index - prevIndex;
			}
			dict[lastSpoken] = index;
			return spoken;
		}
	}
}