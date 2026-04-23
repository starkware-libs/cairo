window.BENCHMARK_DATA = {
  "lastUpdate": 1776942684421,
  "repoUrl": "https://github.com/starkware-libs/cairo",
  "entries": {
    "Cairo Compiler Benchmarks": [
      {
        "commit": {
          "author": {
            "name": "orizi",
            "username": "orizi",
            "email": "104711814+orizi@users.noreply.github.com"
          },
          "committer": {
            "name": "GitHub",
            "username": "web-flow",
            "email": "noreply@github.com"
          },
          "id": "a6a4c78c63ec73d143fe190d98e507b6e458dc15",
          "message": "refactor(lowering): Using `as_mut` instead of `take` + assignment. (#9867)",
          "timestamp": "2026-04-23T10:53:15Z",
          "url": "https://github.com/starkware-libs/cairo/commit/a6a4c78c63ec73d143fe190d98e507b6e458dc15"
        },
        "date": 1776942683007,
        "tool": "cargo",
        "benches": [
          {
            "name": "compile/fib: cairo-to-sierra",
            "value": 1518338624,
            "range": "± 15574596",
            "unit": "ns/iter"
          },
          {
            "name": "compile/fib: cairo-to-diagnostics",
            "value": 926263930,
            "range": "± 25276675",
            "unit": "ns/iter"
          },
          {
            "name": "compile/fib: cairo-to-cache",
            "value": 287063696,
            "range": "± 6711442",
            "unit": "ns/iter"
          },
          {
            "name": "compile/fib: cache-to-sierra",
            "value": 1511434165,
            "range": "± 9682541",
            "unit": "ns/iter"
          }
        ]
      }
    ]
  }
}