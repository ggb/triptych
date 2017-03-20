defmodule Triptych.Mixfile do
  use Mix.Project

  def project do
    [ app: :triptych,
      version: "0.0.1",
      elixir: ">= 1.3.4",
      deps: deps ]
  end

  # Configuration for the OTP application
  def application do
    [
      mod: { Triptych, { Map.new([]), Map.new([]), Map.new([]) } },
      registered: [ :triptych_memo ]
    ]
  end

  # Returns the list of dependencies in the format:
  # { :foobar, git: "https://github.com/elixir-lang/foobar.git", tag: "0.1" }
  #
  # To specify particular versions, regardless of the tag, do:
  # { :barbat, "~> 0.1", github: "elixir-lang/barbat.git" }
  defp deps do
    []
  end
end
