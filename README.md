tryfs-intellisense
==================

A proof of concept intellisense server, intended for http://fsnotebook.net.

To run the demo in the src/Intellisense.Server directory:

 1. Build Intellisense.Server.fsproj
 2. Serve the directory using ASP.NET. For instance, add it as a web application under IIS, or run
    Mono's xsp4 tool.
 3. View index.html in your browser. For instance, with Mono xsp4, go to http://localhost:8080
 4. Type some (valid) F# code. Press Ctrl+Space to see a list of completions. (Wait a few seconds
    the first time, to let the Nancy framework load.)

The REST API is based on the spec here, a work in progress: http://docs.fsnotebook.apiary.io
