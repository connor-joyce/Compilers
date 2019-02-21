let 
  ni x is 0
in
  (while (x < 5) do
    (print(x); now x is x + 1) end; 

  with i as x+0 to 10 do
    (print(i); break) end)
end
