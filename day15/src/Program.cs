using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;

namespace ConsoleApp1
{
	class Program
	{
		private static readonly List<int> Spoken = new List<int>{13, 16, 0, 12, 15, 1};
		private static int Target = 30000000;

		static void Main(string[] args)
		{
			var sw = Stopwatch.StartNew();

			var cache = new Dictionary<int, int>();
			for (var j = 0; j < Spoken.Count - 1; j++)
			{
				cache[Spoken[j]] = j + 1;
			}

			var i = Spoken.Count;
			while (i < Target)
			{
				Turn(i++, Spoken, cache);
			}

			sw.Stop();

			Console.WriteLine(Spoken.Last());
			Console.WriteLine("Took {0} secs", sw.Elapsed.TotalSeconds);
		}

		private static void Turn(int index, ICollection<int> spoken, IDictionary<int, int> dict)
		{
			var lastSpoken = spoken.Last();

			if (dict.ContainsKey(lastSpoken))
			{
				spoken.Add(index - dict[lastSpoken]);
			}
			else
			{
				spoken.Add(0);
			}

			dict[lastSpoken] = index;
		}
	}
}
