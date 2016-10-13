
topic = function () 
{
    if (!exists("topicslist")) 
        data(topicslist)
    sample(topicslist, size = 1)
}

