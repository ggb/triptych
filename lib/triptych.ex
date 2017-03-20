defmodule Triptych do

  # See http://elixir-lang.org/docs/stable/Application.Behaviour.html
  # for more information on OTP Applications
  def start(_type, args) do
    Triptych.Supervisor.start_link(args) 
  end
end
