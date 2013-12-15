
defmodule Triptych.Supervisor do

  use Supervisor.Behaviour

  def start_link(init) do
    result = { :ok, sup } = :supervisor.start_link(__MODULE__, [init])
    start_workers(sup, init)
    result
  end
  
  def start_workers(sup, init) do
    # start stash worker
    { :ok, stash } = :supervisor.start_child(sup, worker(Triptych.Stash, [init]))
    IO.puts "stash ok"
    # start triple store supervisor
    :supervisor.start_child(sup, supervisor(Triptych.CrudSupervisor, [stash]))
    IO.puts "crud ok"
  end

  def init(_) do
    # children = [
      # Define workers and child supervisors to be supervised
      # worker(Triptych.Worker, [])
    # ]

    # See http://elixir-lang.org/docs/stable/Supervisor.Behaviour.html
    # for other strategies and supported options
    supervise([], strategy: :one_for_one)
  end
end
