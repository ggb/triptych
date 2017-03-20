defmodule Triptych.CrudSupervisor do

  use Supervisor
  
  def start_link(stash_pid) do
    :supervisor.start_link(__MODULE__, stash_pid)
  end
  
  def init(stash_pid) do
    child_processes = [ 
      worker(Triptych.Crud, [stash_pid]) 
    ]
    supervise child_processes, strategy: :one_for_one
  end


end
