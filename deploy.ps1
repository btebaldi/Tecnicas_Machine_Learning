# Define the source files and their destination structure
$filesToCopy = @(
@{ Source = ".\01-Syllabus\2024_1_Machine_Learning_1_Guia_do_Estudante.pdf"; Destination = "Tecnicas de ML1 - Guia_do_Estudante.pdf" },
@{ Source = ".\01-Syllabus\Syllabus - Tecnicas de Machine Learning.pdf"; Destination = "Tecnicas de ML1 - Syllabus.pdf" },
@{ Source = ".\02-Slides\2024_1_Machine_Learning_1_slides_.pdf"; Destination = "Tecnicas de ML1 - Slides.pdf" },
@{ Source = ".\03-Database"; Destination = ".\03-Database" },
@{ Source = ".\04-Lista\Lista de exercicio - Tecnicas_de_ML1.pdf"; Destination = "Tecnicas de ML1 - Lista.pdf" },
@{ Source = ".\05-Workbook\2024_1_Machine_Learning_1_Workbook_ (Alunos).pdf"; Destination = "Tecnicas de ML1 - Workbook.pdf" },

@{ Source = ".\20-Forum\Forum 2 - Aluguel de Bicicletas"; Destination = ".\Forum 2 - Aluguel de Bicicletas" },
@{ Source = ".\20-Forum\Forum 1 - Cogumelos Venenosos"; Destination = ".\Forum 1 - Cogumelos Venenosos" }
)


$destinationHomeDir = "$env:USERPROFILE\Downloads\Tecnicas de Machine Learning"

if (-not (Test-Path -Path $destinationHomeDir)) {
    Write-Host "Creating destination directory $destinationHomeDir" -ForegroundColor Yellow
    New-Item -ItemType Directory -Path $destinationHomeDir -Force | Out-Null
}

# Loop through each file and copy it to the specified destination
foreach ($file in $filesToCopy) {
    $source = $file.Source
    $destination = Join-Path -Path $destinationHomeDir -ChildPath $file.Destination

    # Ensure the destination directory exists
    $destinationDir = Split-Path -Path $destination -Parent
    if (-not (Test-Path -Path $destinationDir)) {
        Write-Host "Creating directory $destinationDir"
        New-Item -ItemType Directory -Path $destinationDir -Force | Out-Null
    } 

    # Copy the file
    if (Test-Path -Path $source) {
        Copy-Item -Path $source -Destination $destination -Recurse -force
        Write-Host "Copied $source to $destination"
    } else {
        Write-Host "Source file $source does not exist" -ForegroundColor Red
    }
}