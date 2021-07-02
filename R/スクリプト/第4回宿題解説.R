year <- 2100
if(year%%4==0)
{
  if((year%%100==0)&&(year%%400!=0))
  {
    print("No.")
  }else
  {
    print("Yes.")
  }
}else
{
  print("No.")
}