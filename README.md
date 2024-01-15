

# Macro Cube
The macro perform a balanced sampling of a population based on specified parameters. 
Conducts a balanced draw of a sample.

## Syntax:

```sas

%Cube(
   Pop=		,
   Cons=	,
   Alloc=	,
   DataOut=	,
   land=	,
   seed=
);
```

## Parameters:

### Input:

Pop (file): Required file. Contains the population from which a sample is drawn. Format: UnitId, <prob>  

Cons (file): Contains linear constraints that the drawn sample must satisfy. Format: ConsId, (var1), <(var2), ….,(varN)>  

Alloc (file): Contains the number of units to be drawn from each specified stratum. Format: StratId, Size  

Land (Opt|Drop): Landing method.  

Seed (An integer >=1): Default=1. Source of random numbers used for sampling.  

### Output:

DataOut (file): Default = _popOut. Contains the drawn sample. Format: UnitId  
Note:
If the Alloc file is provided, then the prob variable from the Pop file is not required. If the Alloc file is not provided, then the prob variable from the Pop file is required.
