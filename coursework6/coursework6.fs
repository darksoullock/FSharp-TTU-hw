module coursework6

    open NUnit.Framework
    open FsCheck.NUnit
    open FsCheck    

    // Task 1:

    [<TestFixture>]
    type ``list tests`` () =

        [<Property>]
        member this.
            xsYsLenIsXsLenYsLen (xs: int list) (ys: int list) =
                (xs@ys).Length = xs.Length + ys.Length 

        [<Property>]
        member this.
            xsYsRevIsXsRevYsRev (xs: int list) (ys: int list) =
                ys@xs |> List.rev = (xs |> List.rev) @ (ys |> List.rev)


    // Task 2:

    let rec isPalindrome xs =
        match xs with
        | []        -> true
        | (x :: xs) -> match List.rev xs with
                            | []        -> true
                            | (y :: ys) -> x = y && isPalindrome ys

    [<TestFixture>]
    type ``palindrome tests`` () =

        // a)
        [<Property>]
        member this.
            palindromeIsItReversed (xs: int list) = 
                isPalindrome xs ==> isPalindrome (List.rev xs)

        // b)
        [<Property>]
        member this.
            palindromeIsItReversedStats (xs: int list) = 
                isPalindrome xs ==> isPalindrome (List.rev xs)
                |> Prop.collect (List.length xs)

    // Task 3:

    [<TestFixture>]
    type ``palindrome non-exhaustive tests`` () =

        let toPalindrome xs =
            let len       = List.length xs
            let suffixLen = len / 2
            let prefixLen = if 2 * suffixLen = len then suffixLen else suffixLen + 1
            let take n xs = Seq.toList (Seq.take n xs)
            take prefixLen xs @ List.rev (take suffixLen xs)

        // a)
        
        let prev = (new ``palindrome tests``())

        let palindromeList = Arb.from<list<int>> |> Arb.mapFilter toPalindrome isPalindrome
        
        [<Property>]
        member this.
            palindromeIsItReversedArb() =
                Prop.forAll palindromeList (new ``palindrome tests``()).palindromeIsItReversed

        // b)
        [<Property>]
        member this.
            palindromeIsItReversedArbStats() =
                Prop.forAll palindromeList (new ``palindrome tests``()).palindromeIsItReversedStats

    // Task 5:

    type Client = 
      { Name : string; Income : int ; YearsInJob : int
        UsesCreditCard : bool;  CriminalRecord : bool }

    type QueryInfo =
      { Title     : string
        Check     : Client -> bool
        Positive  : Decision
        Negative  : Decision }

    and Decision = 
       | Result of string
       | Query  of QueryInfo

    let rec tree =
       Query  {Title = "More than €40k"
               Check = (fun cl -> cl.Income > 40000)
               Positive = moreThan40
               Negative = lessThan40}
    and moreThan40 =
       Query  {Title = "Has criminal record"
               Check = (fun cl -> cl.CriminalRecord)
               Positive = Result "NO"
               Negative = Result "YES"}
    and lessThan40 =
       Query  {Title = "Years in job"
               Check = (fun cl -> cl.YearsInJob > 1)
               Positive = Result "YES"
               Negative = usesCreditCard}
    and usesCreditCard =
       Query  {Title = "Uses credit card"
               Check = (fun cl -> cl.UsesCreditCard)
               Positive = Result "YES"
               Negative = Result "NO"}

    let rec testClientTree client tree =
        match tree with
        | Result msg  -> printfn " OFFER A LOAN: %s" msg
                         msg
        | Query qinfo -> let result, case = 
                             if qinfo.Check(client) then
                                 "yes", qinfo.Positive
                             else
                                 "no", qinfo.Negative
                         printfn " - %s ? %s" qinfo.Title result
                         testClientTree client case

    [<TestFixture>]
    type ``tree tests`` () =
        
        [<Test>]
        member this.
            nonCriminal() = 
                let john = {Name = "Allyson Meraz"; Income = 50000 ; YearsInJob = 1 ; 
                    UsesCreditCard = true ; CriminalRecord = false }
                let result = testClientTree john tree
                Assert.AreEqual(result, "YES")

        [<Test>]
        member this.
            criminal() = 
                let john = {Name = "Lana Wilderman"; Income = 50000 ; YearsInJob = 1 ; 
                    UsesCreditCard = true ; CriminalRecord = true }
                let result = testClientTree john tree
                Assert.AreEqual(result, "NO")

        [<Test>]
        member this.
            longYearsInJob() = 
                let john = {Name = "Brandy Fleischman"; Income = 30000 ; YearsInJob = 11 ; 
                    UsesCreditCard = true ; CriminalRecord = false }
                let result = testClientTree john tree
                Assert.AreEqual(result, "YES")

        [<Test>]
        member this.
            creditCard() = 
                let john = {Name = "Fermina Mcclung"; Income = 30000 ; YearsInJob = 1 ; 
                    UsesCreditCard = true ; CriminalRecord = false }
                let result = testClientTree john tree
                Assert.AreEqual(result, "YES")

        [<Test>]
        member this.
            noCreditCard() = 
                let john = {Name = "Paulita Host"; Income = 30000 ; YearsInJob = 1 ; 
                    UsesCreditCard = false ; CriminalRecord = false }
                let result = testClientTree john tree
                Assert.AreEqual(result, "NO")



        [<Property>]
        member this.
            itDoesNotCrash name income years card criminal =
                let john = {Name = name; Income = income; YearsInJob = years ; 
                    UsesCreditCard = card ; CriminalRecord = criminal }
                let result = testClientTree john tree
                Assert.IsTrue(result = "NO" || result = "YES")

        [<Property>]
        member this.
            anyName name = 
                let john = {Name = name; Income = 30000 ; YearsInJob = 1 ; 
                    UsesCreditCard = true ; CriminalRecord = false}
                let result = testClientTree john tree
                Assert.AreEqual(result, "YES")

        [<Property>]
        member this.
            anyCriminal criminal = 
                let john = {Name = "Adam Smith"; Income = 30000 ; YearsInJob = 1 ; 
                    UsesCreditCard = true ; CriminalRecord = criminal }
                let result = testClientTree john tree
                Assert.AreEqual(result, "YES")

        [<Property>]
        member this.
            anyIncome income = 
                let john = {Name = "Adam Smith"; Income = income; YearsInJob = 2 ; 
                    UsesCreditCard = true ; CriminalRecord = false }
                let result = testClientTree john tree
                Assert.AreEqual(result, "YES")

        [<Property>]
        member this.
            whateverCard card = 
                let john = {Name = "Adam Smith"; Income = 50000 ; YearsInJob = 1 ; 
                    UsesCreditCard = card; CriminalRecord = false}
                let result = testClientTree john tree
                Assert.AreEqual(result, "YES")

        [<Property>]
        member this.
            whateverYears years = 
                let john = {Name = "Adam Smith"; Income = 50000 ; YearsInJob = years ; 
                    UsesCreditCard = true ; CriminalRecord = false }
                let result = testClientTree john tree
                Assert.AreEqual(result, "YES")
