﻿using System;
using System.Collections.Generic;

namespace dijkstra_csharp
{

    class Tests
    {
        public static readonly List<(int, int, int)> simpletest =
            new List<(int, int, int)>(new (int, int, int)[] { (1, 2, 7), (1, 3, 9),
                                                              (1, 6, 14), (2, 3, 10),
                                                              (2, 4, 15), (3, 4, 11),
                                                              (3, 6, 2), (4, 5, 6), (6, 5, 9)});
        public static readonly List<(int, int, int)> shorttestcycle =
            new List<(int, int, int)>(new (int, int, int)[] { (1, 2, 1), (2, 3, 1), (3, 1, 1) });
        public static readonly List<(int, int, int)> shortbutbigtest =
            new List<(int, int, int)>(new (int, int, int)[] { (1, 2, 1000), (1, 3, 3000),
            (1, 6, 6000), (2, 3, 3000), 
            (2, 4, 4000), (3, 4, 4000), 
            (3, 6, 6000), (4, 5, 5000), (6, 5, 5000)});
        public static readonly List<(int, int, int)> emptytest =
            new List<(int, int, int)>(new (int, int, int)[] { });
        public static readonly List<(int, int, int)> replaytest =
            new List<(int, int, int)>(new (int, int, int)[] { (1, 4, 2), (1,3,4),(1,2,3),(2,6,3),(3,6,6),(4,5,5),(4,6,2),(5,6,7),(5,9,12),(6,5,1),(6,7,8),(6,8,7),(7,10,4),(8,10,3),(9,8,6),(9,10,11),
                          (1,4,2),(1,3,4),(1,2,3),(2,6,3),(3,6,6),(4,5,5),(4,6,2),(5,6,7),(5,9,12),(6,5,1),(6,7,8),(6,8,7),(7,10,4),(8,10,3),(9,8,6),(9,10,11),
                          (1,4,2),(1,3,4),(1,2,3),(2,6,3),(3,6,6),(4,5,5),(4,6,2),(5,6,7),(5,9,12),(6,5,1),(6,7,8),(6,8,7),(7,10,4),(8,10,3),(9,8,6),(9,10,11),
                          (1,4,2),(1,3,4),(1,2,3),(2,6,3),(3,6,6),(4,5,5),(4,6,2),(5,6,7),(5,9,12),(6,5,1),(6,7,8),(6,8,7),(7,10,4),(8,10,3),(9,8,6),(9,10,11),
                          (1,4,2),(1,3,4),(1,2,3),(2,6,3),(3,6,6),(4,5,5),(4,6,2),(5,6,7),(5,9,12),(6,5,1),(6,7,8),(6,8,7),(7,10,4),(8,10,3),(9,8,6),(9,10,11),
                          (1,4,2),(1,3,4),(1,2,3),(2,6,3),(3,6,6),(4,5,5),(4,6,2),(5,6,7),(5,9,12),(6,5,1),(6,7,8),(6,8,7),(7,10,4),(8,10,3),(9,8,6),(9,10,11),
                          (1,4,2),(1,3,4),(1,2,3),(2,6,3),(3,6,6),(4,5,5),(4,6,2),(5,6,7),(5,9,12),(6,5,1),(6,7,8),(6,8,7),(7,10,4),(8,10,3),(9,8,6),(9,10,11),
                          (1,4,2),(1,3,4),(1,2,3),(2,6,3),(3,6,6),(4,5,5),(4,6,2),(5,6,7),(5,9,12),(6,5,1),(6,7,8),(6,8,7),(7,10,4),(8,10,3),(9,8,6),(9,10,11),
                          (1,4,2),(1,3,4),(1,2,3),(2,6,3),(3,6,6),(4,5,5),(4,6,2),(5,6,7),(5,9,12),(6,5,1),(6,7,8),(6,8,7),(7,10,4),(8,10,3),(9,8,6),(9,10,11)});
        public static readonly List<(int, int, int)> largetest =
            new List<(int, int, int)>(new (int, int, int)[] {(1, 5, 60), (1, 7, 96), (1, 8, 77), (1, 10, 48), (1, 15, 89),
                                                            (1, 16, 20), (1, 23, 62), (2, 4, 53), (2, 9, 24), (2, 15, 9),
                                                            (2, 17, 64), (2, 18, 50), (2, 20, 88), (2, 23, 95), (2, 24, 74),
                                                            (2, 27, 82), (3, 5, 43), (3, 7, 33), (3, 11, 41), (3, 14, 80),
                                                            (3, 16, 80), (3, 22, 18), (3, 24, 10), (4, 8, 6), (4, 9, 18),
                                                            (4, 10, 86), (4, 13, 90), (4, 16, 66), (4, 23, 23), (4, 24, 82),
                                                            (4, 25, 50), (4, 26, 9), (4, 27, 86), (4, 28, 30), (4, 30, 73),
                                                            (5, 7, 31), (5, 21, 36), (5, 22, 64), (5, 25, 67), (5, 30, 30),
                                                            (6, 15, 23), (6, 16, 25), (6, 17, 4), (6, 21, 20), (6, 25, 37),
                                                            (6, 30, 81), (7, 9, 38), (7, 10, 38), (7, 18, 61), (7, 19, 10),
                                                            (7, 23, 70), (7, 27, 92), (8, 9, 73), (8, 10, 9), (8, 12, 71),
                                                            (8, 19, 21), (8, 20, 4), (8, 27, 71), (9, 10, 55), (9, 11, 41),
                                                            (9, 16, 8), (9, 18, 13), (9, 19, 46), (9, 20, 75), (9, 21, 74),
                                                            (9, 22, 99), (9, 25, 19), (9, 28, 75), (9, 30, 83), (10, 11, 91),
                                                            (10, 12, 84), (10, 14, 18), (10, 16, 22), (10, 19, 12), (10, 22, 35),
                                                            (10, 23, 43), (10, 26, 48), (10, 27, 62), (11, 14, 71), (11, 27, 75),
                                                            (12, 14, 9), (12, 20, 3), (12, 22, 87), (12, 24, 32), (12, 26, 38),
                                                            (12, 30, 60), (13, 24, 28), (13, 26, 69), (13, 28, 26), (14, 15, 5),
                                                            (14, 17, 97), (14, 19, 19), (14, 20, 9), (14, 22, 66), (14, 23, 52),
                                                            (14, 25, 63), (14, 26, 1), (14, 27, 71), (14, 28, 85), (15, 16, 37),
                                                            (15, 17, 57), (15, 19, 38), (15, 21, 28), (15, 23, 24), (15, 26, 46),
                                                            (15, 27, 98), (15, 30, 26), (16, 18, 37), (16, 21, 29), (16, 29, 98),
                                                            (17, 19, 89), (17, 23, 45), (17, 24, 74), (17, 29, 66), (18, 19, 7),
                                                            (18, 20, 34), (18, 25, 74), (18, 29, 88), (18, 30, 78), (19, 21, 14),
                                                            (19, 22, 5), (19, 23, 96), (19, 25, 15), (19, 26, 50), (20, 21, 4),
                                                            (20, 24, 66), (20, 26, 18), (20, 30, 84), (21, 22, 82), (21, 27, 56),
                                                            (22, 23, 17), (22, 25, 74), (22, 28, 28), (23, 25, 20), (23, 26, 86),
                                                            (23, 30, 80), (24, 26, 68), (24, 30, 56), (25, 29, 32), (26, 27, 13),
                                                            (26, 29, 36), (27, 30, 95), (29, 30, 14) });
        public static List<(int, int, int)> largeramdomtest (int nuber_of_points)
        {
            List<(int, int, int)> largetest = new List<(int, int, int)>();
            for (int i = 1; i <= (nuber_of_points-1); i++)
                for (int j = i + 1; j <= nuber_of_points; j++)
                    if ((new System.Random()).Next(1, 4) == 1)
                        largetest.Add((i, j, (new System.Random()).Next(1, 100)));
                    else largetest.Add((1, 1, 1));
            return largetest;
        }
        static public void PrintList(List<(int, int, int)> list)
        {
            foreach (var elem in list)
                Console.Write(elem.ToString() + "; ");
            Console.WriteLine("");
        }

        public static void Test (string test_name, int begin_point, int end_point, List<(int, int, int)> test_list)
        {
            var timer = System.Diagnostics.Stopwatch.StartNew();
            int result = Dijkstra.DijkstrasResult(begin_point, end_point, test_list);
            timer.Stop();
            Console.WriteLine("Тест: " + test_name);
            Console.WriteLine("");
            PrintList(test_list);
            Console.WriteLine("");
            Console.WriteLine("Результат (расстояние из вершины " + begin_point + " в вершину " + end_point + "): " + result);
            Console.WriteLine("Затраченное время: " + timer.Elapsed);
            Console.WriteLine("");
            Console.WriteLine("=============================================================================");
            Console.WriteLine("");
        }
    }
    class Dijkstra
    {
        static private readonly int INFINITY = 1000000;

        static private bool IsInList(int point, List<(int, int, bool)> visited)
        {
            if (visited.Count == 0)
                return false;
            foreach (var node in visited)
                if (node.Item1 == point)
                    return true;
            return false;
        }
        static private List<(int, int, bool)> CreateVisitedList(int begin_point, List<(int, int, int)> edges)
        {
            List<(int, int, bool)> visited = new List<(int, int, bool)>();
            visited.Add((begin_point, 0, false));
            foreach (var edge in edges)
            {
                if (!IsInList(edge.Item1, visited))
                    visited.Add((edge.Item1, INFINITY, false));
                if (!IsInList(edge.Item2, visited))
                    visited.Add((edge.Item2, INFINITY, false));
            }
            return visited;
        }
        static private bool End(List<(int, int, bool)> visited)
        {
            bool result = true;
            foreach (var elem in visited)
                result &= elem.Item3;
            return result;
        }
        static private (int, int) FindMinLengthPoint(List<(int, int, bool)> visited)
        {
            (int, int) res_point = (1, INFINITY+1);
            foreach (var point in visited)
                if (!point.Item3 && point.Item2 < res_point.Item2)
                    res_point = (point.Item1, point.Item2);
            return res_point;
        }
        static private int LengthFromP1toP2(int beg_point, int end_point, List<(int, int, int)> edges)
        {
            int result = INFINITY;
            foreach (var edge in edges)
                if (edge.Item1 == beg_point && edge.Item2 == end_point)
                    result = edge.Item3;
            return result;
        }
        static private List<(int, int, bool)> AddPoitToVisited(int point, List<(int, int, bool)> visited)
        {
            for (int i = 0; i < visited.Count; i++)
                if (visited[i].Item1 == point)
                    visited[i] = (point, visited[i].Item2, true);
            return visited;
        }
        static private List<(int, int, bool)> DijkstrasAlgorithm(int begin_point, List<(int, int, int)> edges)
        {
            List<(int, int, bool)> visited = CreateVisitedList(begin_point, edges);
            while (!End(visited))
            {
                (int, int) minpoint = FindMinLengthPoint(visited);
                visited = AddPoitToVisited(minpoint.Item1, visited);
                for (int i = 0; i < visited.Count; i++)
                {
                    if (!visited[i].Item3)
                    {
                        int maybelesslength = minpoint.Item2 + LengthFromP1toP2(minpoint.Item1, visited[i].Item1, edges);
                        if (visited[i].Item2 > maybelesslength)
                            visited[i] = (visited[i].Item1, maybelesslength, false);
                    }
                }
            }
            return visited;
        }

        static public int DijkstrasResult (int begin_point, int end_point, List<(int, int, int)> edges)
        {
            if (edges.Count == 0)
                return -1;
            var result_list = DijkstrasAlgorithm(begin_point, edges);
            foreach (var res in result_list)
                if (res.Item1 == end_point && res.Item2 != INFINITY)
                    return res.Item2;
            return -1;
        }
    }
    class Program
    {
        static void Main(string[] args)
        {
            Tests.Test("Маленький граф с циклом", 1, 3, Tests.shorttestcycle);
            Tests.Test("Граф из википедии", 1, 5, Tests.simpletest);
            Tests.Test("Маленький граф с циклом", 1, 3, Tests.shorttestcycle);
            Tests.Test("Маленький граф с большими цифрами", 1, 5, Tests.shortbutbigtest);
            Tests.Test("Пустой граф", 1, 3, Tests.emptytest);
            Tests.Test("Граф с повторениями", 1, 10, Tests.replaytest);
            Tests.Test("Большой граф", 1, 30, Tests.largetest);
            var largerandomtest = Tests.largeramdomtest(100);
            Tests.Test("Большой случайный граф", 1, 100, largerandomtest);
        }
    }
}
