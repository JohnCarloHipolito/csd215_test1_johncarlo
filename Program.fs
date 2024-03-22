printfn "PART 1\n"

//Create a list that displays the salaries of a company's employees.
//For this list, let salaries = [$75,000; $48,000; $120,000; $190,000; $300,113; $92,000; $36,000].
let salaries = [ 75000; 48000; 120000; 190000; 300113; 92000; 36000 ]
printfn $"Employees gross income %A{salaries}"

//Filter through the list to find high-income salaries. For this list, salaries above $100,000 are considered high.
let highIncomeSalaries = salaries |> List.filter (fun income -> income > 100000)
printfn $"Employees with high income %A{highIncomeSalaries}"

//Use the map function to calculate tax for all salaries based on the table provided.
let taxRate salary =
    if salary <= 49020.0 then 0.15
    elif salary <= 98040.0 then 0.205
    elif salary <= 151978.0 then 0.26
    elif salary <= 216511.0 then 0.29
    else 0.33
let netSalaries =
    salaries
    |> List.map double
    |> List.map (fun income -> income - (income * (taxRate income)))
    |> List.map (fun income -> System.Math.Round(income, 2))
printfn $"Employees income after tax %A{netSalaries}"

//Filter salaries less than $49,020 and add $20,000 to these salaries using the map function.
let lowSalaries =
    salaries
    |> List.filter (fun income -> income < 49020)
    |> List.map (fun income -> income + 20000)
printfn $"Employees income with less than $49,020 added with $20,000 %A{lowSalaries}"

//Filter salaries between $50,000 and $100,000 and sum them all using the reduce/fold function.
let sumOf50KTo100K =
    salaries
    |> List.filter (fun income -> income >= 50000 && income <= 100000)
    |> List.reduce (fun acc income -> acc + income)
printfn $"Sum of employees income between $50,000 and $100,000 is {sumOf50KTo100K}"

printfn "\n\nPART 2\n"

// Use tail recursion to write a program that will calculate the sum of all multiples of 3 up to a given number.
//Assume that we pass only a multiple of 3 as a parameter to this function. No validation is needed.
let rec calcSumOf3s multOf3s sumOf3s =
    if multOf3s = 0 then
        sumOf3s
    else
        calcSumOf3s (multOf3s - 3) (sumOf3s + multOf3s)

//Example: If the parameter is 27, then the result of the function should be: 3+6+9+12+15+18+21+24+27 = 135
let multipleOf3 = 27
let sumOfMultipleOf3s = calcSumOf3s multipleOf3 0
printfn $"Sum of multiples of 3 up to {multipleOf3} is {sumOfMultipleOf3s}"
