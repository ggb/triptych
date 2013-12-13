defmodule TriptychTest do
  use ExUnit.Case
  
  Triptych.Crud.start_link []
  Triptych.Crud.add { :pony, :color, :brown }
  Triptych.Crud.add { :house, :color, :red }
  Triptych.Crud.add { :pony, :age, 9 }
  Triptych.Crud.add { :random, "age", 9 }
  
  
  test "it is possible to add triples" do
    Triptych.Crud.add { :squirrel, :color, :brown }
    assert(Triptych.Crud.find({ :squirrel, :color, :brown }) == [{ :squirrel, :color, :brown }])
  end
  
  test "querying triples works fine. First example. " do
    assert((Triptych.Crud.find({ :blank, :color, :blank }) |> Enum.sort) == ([{ :squirrel, :color, :brown }, { :pony, :color, :brown }, { :house, :color, :red }] |> Enum.sort ))
  end   
  
  test "querying triples works fine. Second example. " do
    assert((Triptych.Crud.find({ :blank, :color, :brown }) |> Enum.sort) == ([{ :squirrel, :color, :brown }, { :pony, :color, :brown }] |> Enum.sort ))
  end 
  
  test "querying triples works fine. Third example. " do
    assert((Triptych.Crud.find({ :pony, :blank, :blank }) |> Enum.sort) == ([{ :pony, :color, :brown }, { :pony, :age, 9 }] |> Enum.sort ))
  end   
  
  test "querying triples works fine. Forth example. " do
    assert(Triptych.Crud.find({ :pony, :age, :blank }) == [{ :pony, :age, 9 }])
  end 
  
  test "tt is possible to get all triples at once" do
    assert((Triptych.Crud.all |> Enum.count) == 5 )
  end
  
  test "if you add a triple twice, you will only find it once" do
    Triptych.Crud.add { :squirrel, :color, :brown }
    assert(Triptych.Crud.find({ :squirrel, :color, :brown }) == [{ :squirrel, :color, :brown }])
  end
    
end
