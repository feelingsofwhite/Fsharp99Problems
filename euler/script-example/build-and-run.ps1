# https://msdn.microsoft.com/en-us/library/dd233175.aspx

write-debug "building"
fsc --nologo -a MyAssembly.fs
if (-not $?) { return }
write-debug "running"
fsi --exec file1.fsx test

