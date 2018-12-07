param(
    [String]
    $dir = $null
)

if ($dir) {
  & cd $dir
}

# http://www.dougfinke.com/blog/index.php/2010/08/29/how-to-load-net-assemblies-in-a-powershell-session/
[Reflection.Assembly]::LoadFile('C:\Program Files (x86)\MSBuild\12.0\Bin\amd64\Microsoft.Build.Utilities.v12.0.dll')

$msbuild = [Microsoft.Build.Utilities.ToolLocationHelper]::GetPathToBuildToolsFile(
  "msbuild.exe",
  [Microsoft.Build.Utilities.ToolLocationHelper]::CurrentToolsVersion, #"12.0",
  [Microsoft.Build.Utilities.DotNetFrameworkArchitecture]::Bitness64).ToString()
$msbuild_args = '-nologo', '-version' #, '-c', '-'
# use "&" to execute a string from powershell
& $msbuild $msbuild_args





