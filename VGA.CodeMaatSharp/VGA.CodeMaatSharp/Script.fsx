// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "../.paket/packages/FsLab/FsLab.fsx"

open System
open System.IO
open XPlot.GoogleCharts
open FSharp.Data
open MathNet.Numerics

// git log --pretty=format:'[%H],%aN,%ad,%s' --date=short --numstat > sfa-log.log

// --------------------------
// 1.- PARSE GIT LOG FILE
// --------------------------

[<Literal>]
let LineBreak = "\r\n"

type CommitInfo = {Hash : string; Author : string; TimeStamp : DateTime; Message : string}
type CommittedFile = {LinesAdded: int option; LinesDeleted: int option; FileName: string}
type Commit = {CommitInfo: CommitInfo; Files: CommittedFile[]}

let filePath = Path.Combine(__SOURCE_DIRECTORY__, "..\..\Data\sfa-log.log")
let file = File.ReadAllText(filePath)

type CommitInfoCsv = CsvProvider<"Hash,Author,Date,Message", HasHeaders = false, Schema = "Hash,Author,Date(string),Message">
type CommitLineCsv = CsvProvider<"LinesAdded\tLinesDeleted\tFile", HasHeaders = false, Schema = "LinesAdded(int option),LinesDeleted(int option),FileName">

let commits = file.Split([|LineBreak + LineBreak|], StringSplitOptions.RemoveEmptyEntries)

let extractCommitInfo (commit:string) =
    let isCommitInfoLine (line: string) =
        line.StartsWith("[")

    let extractCommitedFilesInfo c = 
        let commitFileLine = CommitLineCsv.Parse(c).Rows |> Seq.head
        {LinesAdded = commitFileLine.LinesAdded; LinesDeleted = commitFileLine.LinesDeleted; FileName = commitFileLine.FileName}

    let commitLines = commit.Split([|LineBreak|], StringSplitOptions.RemoveEmptyEntries)    
    let commitInfoLine = commitLines |> Array.takeWhile(isCommitInfoLine) |> Array.last
    let fileLines = commitLines |> Array.skipWhile(isCommitInfoLine)
    
    let infoRow = CommitInfoCsv.Parse(commitInfoLine).Rows |> Seq.head
    let commitInfo = {Hash = infoRow.Hash; Author = infoRow.Author; TimeStamp = DateTime.ParseExact(infoRow.Date,"ddd MMM d HH:mm:ss yyyy", System.Globalization.CultureInfo.InvariantCulture); Message = infoRow.Message}

    let fileRows = fileLines |> Array.map extractCommitedFilesInfo
    
    {CommitInfo = commitInfo; Files = fileRows}

// --------------------------
// 2.- BASIC STATISTICS
// --------------------------

// 2.1 - TOTAL COMMITS
let totalCommits = 
    commits
    |> Array.map extractCommitInfo


// 2.2 - NUMBER OF FILES CHANGED ()
let numberOfEntitiesChanged =
    totalCommits
    |> Array.collect(fun c -> c.Files)
    |> Array.length


// 2.3 - NUMBER OF FILE
let numberOfEntities = 
    totalCommits
    |> Array.collect(fun c -> c.Files)
    |> Array.groupBy(fun f -> f.FileName)
    |> Array.length
    

// 2.4 - FILE BY TYPE
let fileByType =
    totalCommits
    |> Array.collect(fun c -> c.Files)
    |> Array.distinctBy(fun f -> f.FileName)
    |> Array.groupBy(fun f -> Path.GetExtension f.FileName)
    |> Array.map(fun f -> fst f, (snd f) |> Array.length)
    |> Array.sortByDescending snd


// 2.5 - CHART OF FYLE BY TYPE
let chartFileByType =
    fileByType
    |> Chart.Pie
    |> Chart.WithLegend true
    |> Chart.WithSize(640, 480)


// 2.6 - GET AUTHORS
let mapAuthors = 
    Map.empty
        .Add("Janusz Kali Kaliszczak", "Janusz 'Kali' Kaliszczak")
        .Add("iterative-it", "David Winchurch")
        .Add("charge-valtech", "Henry Charge")
        .Add("SandeepYadav", "Sandeep Yadav")
        .Add("sandeep-sfa", "Sandeep Yadav")
        .Add("kristerbone", "Krister Bone")
        .Add("Vicen├º Garc├¡a Alt├®s", "Vicenc Garcia-Altes")
        .Add("vgaltes", "Vicenc Garcia-Altes")

let consolidateNames (name:string) =
    match name with
    |_ when mapAuthors.ContainsKey name -> (mapAuthors.TryFind name).Value
    |authorName -> authorName

let authors =
    totalCommits
    |> Array.groupBy(fun c -> c.CommitInfo.Author)
    |> Array.map(fun (a, _) -> a)
    |> Array.map consolidateNames
    |> Array.distinct

let numberOfAuthors =
    authors
    |> Array.length


// --------------------------
// 2.- HOTSPOTS
// --------------------------

// 3.1 - NUMBER OF REVISIONS BY FILE
let numberOfRevisionsByFile =
    totalCommits
    |> Array.collect(fun c -> c.Files)
    |> Array.groupBy(fun f -> f.FileName)
    |> Array.map ( fun c -> fst c, (snd c).Length)
    |> Array.sortByDescending snd
    |> Array.take 10

// 3.2 - BAR CHART OF NUMBER OF REVISIONS
numberOfRevisionsByFile
|> Chart.Bar
|> Chart.WithLabels ["Number of revisions"]

// 3.3 - GET A FILE FROM GITHUB
// https://github.com/SkillsFundingAgency/FindApprenticeship/blob/251074332a2165fa3b9ccbd67b85b183fd4b1d1c/src/SFA.Apprenticeships.Domain.Entities/Vacancies/ProviderVacancies/Apprenticeship/ApprenticeshipVacancy.cs
// https://raw.githubusercontent.com/SkillsFundingAgency/FindApprenticeship/master/src/SFA.Apprenticeships.sln
let gitHubRawContentBaseAddress = "https://raw.githubusercontent.com/SkillsFundingAgency/FindApprenticeship/master/"
let getFileFromGitHub fileUrl =
    let request = Http.Request(fileUrl, silentHttpErrors = true)
    if ( request.StatusCode = 200 ) then
        match request.Body with
        | Text(t) -> Some(t)
        | Binary(_) -> None             
    else None
    
// 3.4 - CALCULATE COMPLEXITY 
// 3.4.1 - VIA NUMBER OF LINES
let listOfFiles =
    totalCommits
    |> Array.collect(fun c -> c.Files)
    |> Array.groupBy(fun f -> f.FileName)
    |> Array.map fst

let numberOfLinesOf file =
    let gitHubPath = Path.Combine ( gitHubRawContentBaseAddress, file)
    let fileContent = getFileFromGitHub gitHubPath
    match fileContent with
    | Some(content) -> Some (file, content.Split([|'\n'|]) |> Array.length)
    | None -> None

let filesToStudy = [|
    "src/SFA.Apprenticeships.Web.Raa.Common/Providers/VacancyProvider.cs";
    "src/SFA.Apprenticeships.Web.Recruit/Controllers/VacancyPostingController.cs";
    "src/SFA.Apprenticeships.Web.Recruit/Mediators/VacancyPosting/VacancyPostingMediator.cs";
    "src/SFA.WebProxy/IO/EchoStream.cs";
    "src/SFA.Apprenticeships.Web.Manage.EndToEndTests/VacancyControllerIntegrationTests.cs";
    "src/SFA.Apprenticeships.Web.Candidate.UnitTests/Views/ApprenticeshipSearch/DetailsTests.cs";
    "src/SFA.Apprenticeships.Web.Candidate/Providers/CandidateServiceProvider.cs";
    "src/SFA.Apprenticeships.Web.Candidate/Providers/ApprenticeshipApplicationProvider.cs";
    "src/SFA.Apprenticeships.Web.Candidate.UnitTests/Mediators/ApprenticeshipSearch/ResultsTests.cs";
    "src/SFA.Apprenticeships.Infrastructure.UnitTests/LegacyWebServices/GatewayApprenticeshipVacancyDetailMapperTests.cs";
    "src/SFA.Apprenticeships.Web.Candidate/Mediators/Search/ApprenticeshipSearchMediator.cs";
    "src/SFA.Apprenticeships.Web.Candidate/Controllers/AccountController.cs";
    "src/SFA.Apprenticeships.Web.Candidate/Controllers/VacancySearchController.cs";
    "src/SFA.Apprenticeships.Web.Manage/Controllers/VacancyController.cs";
    "src/Prototypes/Areas/Recruit/Controllers/VacancyPostingController.cs";
    "src/SFA.Apprenticeships.Application.Candidate/CandidateService.cs";
    "src/SFA.Apprenticeships.Web.Candidate/Controllers/LoginController.cs"|]
    
let numberOfLinesByFile =
    listOfFiles
    |> Array.filter(fun f -> filesToStudy |> Array.contains f)
    |> Array.choose numberOfLinesOf
    |> Array.sortByDescending snd

let getFileNameFromPath (p:string) =
    p.Split('/') |> Array.rev |> Array.head

numberOfLinesByFile
|> Array.map(fun f -> (getFileNameFromPath(fst f), snd f))
|> Chart.Bar
|> Chart.WithLabels ["Number of lines"]

// 3.4.2 - VIA NUMBER OF TABS

// 3.5 - NUMBER OF REVISIONS AND AUTHORS

let extensionBlacklist = [|".css"; ".feature.cs"; ".generated.cs"; ".config"; ".scss"; ".csproj"; ".cshtml"; ".min.js"|]

let filteredCommits =
    totalCommits
    |> Array.map(fun c -> {CommitInfo = c.CommitInfo; 
                           Files = c.Files 
                           |> Array.filter(fun f -> extensionBlacklist 
                                                    |> Array.forall(fun e -> not (f.FileName.EndsWith(e))))})

let numberOfRevisionsPerFile =
    filteredCommits
    |> Array.map(fun c -> c.Files |> Array.filter(fun f -> filesToStudy 
                                                            |> Array.contains f.FileName) 
                                                            |> Array.map(fun f -> (c.CommitInfo.Author, f)))
    |> Array.collect id
    |> Array.groupBy(fun (_,f) -> f.FileName)
    |> Array.map(fun(f, af) -> f, af |> Array.length)
    |> Array.sortByDescending snd

let numberOfAuthorsPerFile =
    filteredCommits
    |> Array.map(fun c -> c.Files |> Array.filter(fun f -> filesToStudy 
                                                            |> Array.contains f.FileName) 
                                                            |> Array.map(fun f -> (c.CommitInfo.Author, f)))
    |> Array.collect(fun x -> x)
    |> Array.map(fun (a,f) -> (consolidateNames a), f)
    |> Array.groupBy(fun (_,f) -> f.FileName)
    |> Array.map(fun(f, af) -> f, af |> Array.groupBy(fun (a,_) -> a) |> Array.length)
    |> Array.sortByDescending snd

// Combined chart
[|numberOfRevisionsPerFile;numberOfAuthorsPerFile|]
|> Chart.Bar
|> Chart.WithLabels ["Number of revisions"; "Number of authors"]


// 4.- COUPLING
let combinator (seq:string[]) = 
    [|
        for i in 0 .. seq.Length - 1 do
            for j in i+1 .. seq.Length - 1  do
                if i <> j then 
                    if seq.[i] < seq.[j] then
                        yield (seq.[i], seq.[j])
                    else yield (seq.[j], seq.[i])
    |]

type CommitedPair = {File1 : string; File2: string; TimesCommitedTogether: int}

let committedTogether =
    //totalCommits
    filteredCommits
    |> Array.map(fun c -> c.Files |> Array.map (fun f -> f.FileName) |> combinator)
    |> Array.collect id
    |> Array.groupBy id
    |> Array.sortByDescending ( fun m -> snd m |> Array.length)
    |> Array.map(fun (k, a) -> {File1 = fst k; 
                                File2 = snd k; 
                                TimesCommitedTogether = (a |> Array.length)})    

committedTogether
|> Array.take 20
|> Array.map(fun c -> getFileNameFromPath c.File1, getFileNameFromPath c.File2, c.TimesCommitedTogether)
|> Chart.Sankey
|> Chart.WithLabels["File1"; "File2"; "Number of times committed together"]

let NumberOfCommitsThreshold = 50

type CommitedFile = { FileName : string; NumberOfCommits : int }

let commitsPerFile =
    // totalCommits
    filteredCommits
    |> Array.collect (fun c -> c.Files)
    |> Array.groupBy (fun c-> c.FileName)
    |> Array.map(fun c -> {FileName = fst c; NumberOfCommits = (snd c |> Array.length)})
    |> Array.sortByDescending (fun cf -> cf.NumberOfCommits)
    |> Array.takeWhile ( fun cf -> cf.NumberOfCommits > NumberOfCommitsThreshold)

type CouplingInformation = {File1: string; File2: string; TimesCommitedTogether: int; CouplingDegree: float}
    
let commitsPerPair = 
    let calculateCoupling (c:CommitedPair) =
        let total = commitsPerFile |> Array.find (fun cf -> cf.FileName = c.File1 )                                       
        {File1 = c.File1; 
        File2 = c.File2; 
        TimesCommitedTogether = c.TimesCommitedTogether; 
        CouplingDegree = float c.TimesCommitedTogether / float total.NumberOfCommits}

    let round (x: float) = int (Math.Round (x * 100.0))

    let sortByCouplingDegree a b =
        let couplingDegree = b.CouplingDegree - a.CouplingDegree
        let numberRevisions = b.TimesCommitedTogether - a.TimesCommitedTogether
        if couplingDegree <> 0.0 then round couplingDegree else numberRevisions
        
    committedTogether
    |> Array.filter ( fun c -> commitsPerFile|> Array.exists (fun cf -> cf.FileName = c.File1 ))                                                 //Map.containsKey (fst (fst c)) )
    |> Array.map calculateCoupling
    |> Array.sortWith sortByCouplingDegree

commitsPerPair
    |> Array.iter (fun r -> printf "File1: %s\nFile2: %s\nNumber of commits: %d\nCoupling: %f\n\n" r.File1 r.File2 r.TimesCommitedTogether r.CouplingDegree)

commitsPerPair
|> Array.take 20
|> Array.map(fun c -> getFileNameFromPath c.File1, getFileNameFromPath c.File2, c.CouplingDegree)
|> Chart.Sankey
|> Chart.WithLabels["File1"; "File2"; "Coupling degree"]

// 5.- COMPLEXITY OVER TIME

let calculateFileStatistics (response: HttpResponseBody) =
    match response with
    | Text(t) -> 
        let srcLines = t.Split([|"\n"|], StringSplitOptions.RemoveEmptyEntries)

        let numLines = srcLines |> Array.length

        let spaces = srcLines |> Array.map ( fun l -> l.ToCharArray() |> Array.takeWhile(fun c -> c = ' '))
        let numTabs = spaces |> Array.map ( fun l -> float( l |> Array.length ) / 4.0)
        let maxTabs = numTabs|> Array.max
        let averageTabs = numTabs |> Array.average

        numLines, maxTabs, averageTabs
    | Binary(_) -> 0, 0., 0.  

//let saveFileToDisk (fileUrl:string) (index: int) (response: HttpResponseBody) =
//    match response with
//    | Text(t) -> 
//        let commitRelativeFilePath = "..\\..\\Data\\" +  Path.GetFileNameWithoutExtension fileUrl + "_" + index.ToString() + "_" + ".txt"
//        let fileName = Path.Combine(__SOURCE_DIRECTORY__, commitRelativeFilePath) 
//        File.WriteAllText(fileName, t)
//    | Binary(b) -> ()   

// "src/SFA.Apprenticeships.Web.Candidate/Controllers/VacancySearchController.cs"
let getHistoryOf file =
    totalCommits
    |> Array.filter(fun cf -> cf.Files |> Array.filter(fun f -> f.FileName.Contains file) |> Array.length > 0)
    |> Array.map(fun cf -> cf.CommitInfo.TimeStamp, cf.CommitInfo.Hash.Substring(1, cf.CommitInfo.Hash.Length - 2))

let getComplexityOf file =
    let githubPath = "https://raw.githubusercontent.com/SkillsFundingAgency/FindApprenticeship/"
   
    getHistoryOf file
    |> Array.map(fun f -> fst f, githubPath + snd f + "/" + file)
    |> Array.choose(fun f -> let request = Http.Request(snd f, silentHttpErrors = true)
                             if ( request.StatusCode = 200 ) then
                                Some (fst f, calculateFileStatistics request.Body)   
                             else None    
                            )
    |> Array.sortBy(fst)


let complexity = 
    getComplexityOf "src/SFA.Apprenticeships.Web.Candidate/Controllers/ApprenticeshipSearchController.cs"

let first (x, _, _) = x
let second (_,x,_) = x
let third (_,_,x) = x
complexity
    |> Array.map(fun f -> fst f, (first (snd f)))
    |> Chart.Line

complexity
    |> Array.map(fun f -> fst f, (second (snd f)))
    |> Chart.Line

complexity
    |> Array.map(fun f -> fst f, (third (snd f)))
    |> Chart.Line




type FileCommits = {FileName: string; CommitHashes: string[]}
let filesWithCommitHash = 
    totalCommits
    |> Array.collect(fun c -> c.Files |> Array.map(fun f -> f.FileName, c.CommitInfo.Hash.Substring(1, c.CommitInfo.Hash.Length - 2)))
    |> Array.filter( fun f -> (fst f).EndsWith(".cs"))
    |> Array.groupBy fst
    |> Array.map(fun c -> {FileName = fst c; CommitHashes = (snd c) |> Array.map snd })
    
filesWithCommitHash
    |> Array.filter (fun f -> f.FileName.Contains("VacancySearchController.cs"))
    |> Array.map(fun f -> 
        f.FileName, f.CommitHashes |> Array.map ( fun c ->
            let fileUri = githubPath + c + "/" + f.FileName
            fileUri
        )        
    )
//    |> Array.iter(fun f -> snd f |> Array.iteri(fun i c -> 
//        let request = Http.Request(c, silentHttpErrors = true)
//        if ( request.StatusCode = 200 ) then
//            saveFileToDisk c i request.Body            
//        )
//    )
    |> Array.choose(fun f -> snd f |> Array.iter(fun c -> 
        let request = Http.Request(c, silentHttpErrors = true)
        if ( request.StatusCode = 200 ) then
            Some (calculateFileStatistics request.Body)   
        else None    
        )        
    )

// Get all the files from disk and calculate the number of lines -> graph
// Get all the files from disk and calculate the number of tabs -> graph
// Get the latest file and calculate complexity (lines or tabs) and authors.

let fileInfos = 
    filesWithCommitHash
    |> Array.filter (fun f -> f.FileName.Contains("VacancySearchController.cs"))
    |> Array.map(fun f -> 
        f.FileName, f.CommitHashes |> Array.map ( fun c ->
            let fileUri = githubPath + c + "/" + f.FileName
            fileUri
        )        
    )
    |> Array.map(fun f -> fst f, snd f |> Array.map(fun c -> 
        let request = Http.Request(c, silentHttpErrors = true)
        if ( request.StatusCode = 200 ) then
            let stats = calculateFileStatistics request.Body
            Some (stats)
        else 
            None
        )
    )
    |> Array.map(fun f -> fst f, snd f |> Array.map (fun s -> 
        match s with
        |Some x -> Some (Statistics.ArrayStatistics.MeanVariance (snd x))
        |None -> None
        ))
    |> Array.collect snd
    |> Array.filter(fun f -> f.IsSome)
    |> Array.toSeq


//type CommitInfo = {Hash : string; Author : string; TimeStamp : DateTime; Message : string}
//type CommittedFile = {LinesAdded: int option; LinesDeleted: int option; FileName: string}
//type Commit = {CommitInfo: CommitInfo; Files: CommittedFile[]}

// 6.- AUTHORS
let commitsPerAuthor =
    totalCommits
    |> Array.map ( fun c -> {CommitInfo = {Hash = c.CommitInfo.Hash; Author = consolidateNames c.CommitInfo.Author; TimeStamp = c.CommitInfo.TimeStamp; Message = c.CommitInfo.Message}; Files = c.Files})
    |> Array.groupBy ( fun c -> c.CommitInfo.Author)
    |> Array.map(fun ca -> (fst ca, (snd ca) |> Array.length))
    |> Array.sortByDescending snd

(**
let data =
    let rnd = Random()
    [
        for x in 1. .. 600. ->
            DateTime(2013, 1, 9).AddDays(x), rnd.Next(0, 5)
    ]

data
|> Chart.Calendar
**)

totalCommits
|> Array.map ( fun c -> {CommitInfo = {Hash = c.CommitInfo.Hash; Author = consolidateNames c.CommitInfo.Author; TimeStamp = c.CommitInfo.TimeStamp; Message = c.CommitInfo.Message}; Files = c.Files})
|> Array.groupBy(fun c -> c.CommitInfo.TimeStamp.Date)
|> Array.map(fun c -> fst c, (snd c) |> Array.length)
|> Chart.Calendar