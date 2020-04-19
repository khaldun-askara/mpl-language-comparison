open System

[<EntryPoint>]
let main argv =
    let dijkstra begin_point end_point list_of_points =
        // "максимальное" значение
        let infinity = 1000000;

        // ищет в таблице результатов нужный
        let find_length end_point result_list = 
            let result = List.filter (fun (point, lenght) -> point = end_point) result_list
            match result with
            | [] -> -1
            | head::_ -> snd head

        // проверяет, была ли вершина посещена
        let rec belongs_to point visited =
            match point, visited with
            | _, [] -> false
            | point, (( curpoint, _)::visited_tail) -> if (point = curpoint) then true else belongs_to point visited_tail
            
        // возвращает список текущих расстояний до вершин
        let rec compound_path arr visited =
            match arr with
            | [] -> []
            | (point1, point2, length)::tail -> 
                    let current_lenght point = List.fold (fun acc elem -> if ((fst elem) = point) then (snd elem) else acc) infinity visited
                    (point2, (current_lenght point1) + length)::(compound_path tail visited)

        // возвращает пару с минимальным путём среди пар (вершина, путь)
        let return_min_couple arr =
            match arr with
            | [] -> (infinity, infinity)
            | _ -> List.fold  (fun acc elem -> if ((snd acc) > (snd elem)) then elem else acc) (infinity, infinity) arr

        // поиск ближайшей соседней вершины
        let check_neighbors point visited list =
            let b = if (belongs_to point visited) then list else []
            let a = List.filter (fun (x, y, z) -> not (belongs_to y visited)) b
            let c = compound_path a visited
            return_min_couple c
        
        // алгоритм Дейкстры, ищет кратчайшие пути от начальной до каждой из вершин, 
        // в visited должна уже быть начальная вершина и длина до неё 0
        let rec dijkstra_ s list visited =
            let tempcouple = check_neighbors s visited list
            if (fst tempcouple = infinity && snd tempcouple = infinity)
            then visited
            else dijkstra_ (fst tempcouple) list (tempcouple::visited)

        find_length end_point (dijkstra_ begin_point list_of_points [(begin_point, 0)])
    
    let tests =
        // ====================================== тесты ======================================
        // простой тест со графом из википедии
        let simpletest = [(1, 2, 7); (1, 3, 9);
        (1, 6, 14); (2, 3, 10); 
        (2, 4, 15); (3, 4, 11); 
        (3, 6, 2); (4, 5, 6); (6, 5, 9)]
        // маленький граф с циклом
        let shorttestcycle = [(1, 2, 1); (2, 3, 1); (3, 1, 1)]
        // маленький граф с большими цифрами
        let shortbutbigtest = [(1, 2, 1000); (1, 3, 3000);
            (1, 6, 6000); (2, 3, 3000); 
            (2, 4, 4000); (3, 4, 4000); 
            (3, 6, 6000); (4, 5, 5000); (6, 5, 5000)]
        // пустой граф
        let emptytest = []
        // граф с повторениями
        let replaytest = [(1,4,2);(1,3,4);(1,2,3);(2,6,3);(3,6,6);(4,5,5);(4,6,2);(5,6,7);(5,9,12);(6,5,1);(6,7,8);(6,8,7);(7,10,4);(8,10,3);(9,8,6);(9,10,11);
                          (1,4,2);(1,3,4);(1,2,3);(2,6,3);(3,6,6);(4,5,5);(4,6,2);(5,6,7);(5,9,12);(6,5,1);(6,7,8);(6,8,7);(7,10,4);(8,10,3);(9,8,6);(9,10,11);
                          (1,4,2);(1,3,4);(1,2,3);(2,6,3);(3,6,6);(4,5,5);(4,6,2);(5,6,7);(5,9,12);(6,5,1);(6,7,8);(6,8,7);(7,10,4);(8,10,3);(9,8,6);(9,10,11);
                          (1,4,2);(1,3,4);(1,2,3);(2,6,3);(3,6,6);(4,5,5);(4,6,2);(5,6,7);(5,9,12);(6,5,1);(6,7,8);(6,8,7);(7,10,4);(8,10,3);(9,8,6);(9,10,11);
                          (1,4,2);(1,3,4);(1,2,3);(2,6,3);(3,6,6);(4,5,5);(4,6,2);(5,6,7);(5,9,12);(6,5,1);(6,7,8);(6,8,7);(7,10,4);(8,10,3);(9,8,6);(9,10,11);
                          (1,4,2);(1,3,4);(1,2,3);(2,6,3);(3,6,6);(4,5,5);(4,6,2);(5,6,7);(5,9,12);(6,5,1);(6,7,8);(6,8,7);(7,10,4);(8,10,3);(9,8,6);(9,10,11);
                          (1,4,2);(1,3,4);(1,2,3);(2,6,3);(3,6,6);(4,5,5);(4,6,2);(5,6,7);(5,9,12);(6,5,1);(6,7,8);(6,8,7);(7,10,4);(8,10,3);(9,8,6);(9,10,11);
                          (1,4,2);(1,3,4);(1,2,3);(2,6,3);(3,6,6);(4,5,5);(4,6,2);(5,6,7);(5,9,12);(6,5,1);(6,7,8);(6,8,7);(7,10,4);(8,10,3);(9,8,6);(9,10,11);
                          (1,4,2);(1,3,4);(1,2,3);(2,6,3);(3,6,6);(4,5,5);(4,6,2);(5,6,7);(5,9,12);(6,5,1);(6,7,8);(6,8,7);(7,10,4);(8,10,3);(9,8,6);(9,10,11);]
        // большой граф
        let largetest =
            [for i in 1 .. 99 do
                for j in (i+1) .. 100 ->
                    if ((new System.Random()).Next(1, 3) = 1)
                    then (1, 1, 1)
                    else (i, j, (new System.Random()).Next(1, 100))]
        
        // выводит список
        let rec print_list list acc =
            match list with
            | [] -> acc
            |(head::tail) ->
                    printf "%A" head
                    print_list tail acc+1
        // запускает тесты
        let test test_name begin_point end_point test_list =
            let timer = System.Diagnostics.Stopwatch.StartNew()
            let result = dijkstra begin_point end_point test_list
            timer.Stop()
            printfn "Тест: %s" test_name
            print_list test_list 0 |> ignore
            printfn ""
            printfn "Результат (расстояние из вершины %A в вершину %A: %A" begin_point end_point result
            printfn "Затраченное время: %O" timer.Elapsed
            printfn ""
            printfn ""
            0
        test "Граф из википедии" 1 6 simpletest |> ignore
        test "Маленький граф с циклом" 1 3 shorttestcycle |> ignore
        test "Маленький граф с большими цифрами" 1 3 shortbutbigtest |> ignore
        test "Пустой граф" 1 1 emptytest |> ignore
        test "Граф с повторениями" 1 10 replaytest |> ignore
        test "Большой случайный граф" 1 100 largetest |> ignore
        0

    let res = tests

    0
