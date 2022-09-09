using Documenter

push!(LOAD_PATH, "../src/")
using QuickHeaps

DEPLOYDOCS = (get(ENV, "CI", nothing) == "true")

makedocs(
    sitename = "Efficient and versatile binary heaps and priority queues for Julia",
    format = Documenter.HTML(
        prettyurls = DEPLOYDOCS,
    ),
    authors = "Éric Thiébaut and contributors",
    pages = ["index.md", "install.md", "binaryheaps.md",
         "priorityqueues.md", "nodes.md", "library.md"]
)

if DEPLOYDOCS
    deploydocs(
        repo = "github.com/emmt/QuickHeaps.jl.git",
    )
end
