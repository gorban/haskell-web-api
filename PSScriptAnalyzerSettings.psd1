@{
    # Exclude the alias rule because we have a polyglot generate-code-coverage.ps1 script
    # that needs to use the word "echo" to start the bash versus PowerShell split.
    ExcludeRules = @(
        'PSAvoidUsingCmdletAliases',
        'PSAvoidUsingWriteHost'
    )
}
