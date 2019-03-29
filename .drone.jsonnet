local Pipeline(version) = {
  kind: "pipeline",
  name: version,
  steps: [
    {
      name: "test",
      image: "haskell:"+version,
      commands: [
        "cabal new-update || cabal update",
        "cabal new-build all"
      ]
    }
  ]
};

[
  Pipeline("8"),
  Pipeline("8.2"),
  Pipeline("8.4"),
  Pipeline("8.6"),
]

