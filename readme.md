# Specification
             
## Is
        
### Case 1.1.1:
input = `find person whose @name is Maksym`
```
output = [
    {
        "name": {
            operator: "is"
            value: "Maksym"
        }
    }
] 
```       

### Case 1.1.2:
input = `find person whose @name is "Maksym"`

```
output = [
    {
        "name": {
            operator: "is"
            value: "Maksym"
        }
    }    
]
```
   
### Case 1.1.3:
input = `find person whose @name is "Maksym Oliinyk"`
```
output = [
    {
        "name": {
            operator: "is"
            value: "Maksym Oliinyk"
        }
    }    
]
```
    
### Case 1.1.4:
input = `@name is "Maksym Oliinyk"`    
```
output = [
    {
        "name": {
            operator: "is"
            value: "Maksym Oliinyk"
        }
    }
]    
```
    
### Case 1.1.5:
input = `find person with name Maksym`    
```
output = [
]        
```
    
### Case 1.1.6:
input = `find person whose name is Maksym`    
```
output = [
]        
```

### Case 1.1.7:
input = `find person whose @ name is Maksym`    
```
output = [
]        
```
    
## Is not

### Case 1.2.1:
input = `find person whose @name is not Maksym`    
```
output = [
    {
        "name": {
            operator: "is not"
            value: "Maksym"
        }
    }    
]    
```

### Case 1.2.2:
input = `find person whose @name is not "Maksym"`    
```
output = [
    {
        "name": {
            operator: "is not"
            value: "Maksym"
        }
    }    
]    
```

### Case 1.2.3:
input = `find person whose @name is not Maksym Oliinyk`    
```
output = [
    {
        "name": {
            operator: "is not"
            value: "Maksym Oliinyk"
        }
    }
]        
```

### Case 1.2.4:
input = `find person whose @name is not "Maksym Oliinyk"`    
```
output = [
    {
        "name": {
            operator: "is not"
            value: "Maksym Oliinyk"
        }
    }
]        
```

## Either or / Neither nor

### Case 1.3.1:
input = `@forename is either Maksym or Viktor`    
```
output = [
    {
        forename: {
            operator: "is in",
            value: [
                "Maksym", 
                "Viktor"
            ]
        }
    }
]     
```

### Case 1.3.2:
input = `@forename is either Maksym or Viktor or Julia or ...`    
```
output = [
    {
        forename: {
            operator: "is in",
            value: ["Maksym", "Viktor", "Julia", ... ]
        }
    }
]     
```

### Case 1.3.3:
input = `@forename is neither Maksym nor Viktor`    
```
output = [
    {
        forename: {
            operator: "is not in",
            value: ["Maksym", "Viktor"]
        }
    }
]     
```

### Case 1.3.4:
input = `@forename is neither Maksym nor Viktor nor Julia nor ...`    
```
output = [
    {
        forename: {
            operator: "is not in",
            value: ["Maksym", "Viktor", "Julia", ... ]
        }
    }
]     
```

## And / Or

### Case 1.4.1:
input = `find person whose @forename is Maksym and @surname Oliinyk`    
```
output = [
    {
        forename: {
            operator: "is",
            value: "Maksym"
        },
        surname: {
            operator: "is",
            value: "Oliinyk"
        }
    }
]     
```

### Case 1.4.2:
input = `@forename is Maksym and @surname is Oliinyk`    
```
output = [
    {
        forename: {
            operator: "is",
            value: "Maksym"
        },
        surname: {
            operator: "is",
            value: "Oliinyk"
        }
    }
]     
```

### Case 1.4.3:
input = `@forename is in (Maksym, Viktor) or @forename is in (Alex, Julia)`    
```
output = [
    {
        forename: {
            operator: "is in",
            value: ["Maksym", "Viktor"]
        }
    },
    {
        forename: {
            operator: "is in",
            value: ["Alex", "Julia"]
        }
    }
]     
```

### Case 1.3.4:
input = `@forename is Maksym and @surname is either Ivanov or Petrov`    
```
output = [
    {
        forename: {
            operator: "is",
            value: "Maksym"
        },
        surname: {
            operator: "is in",
            value: ["Ivanov", "Petrov"] 
        }
    }
]     
```

### Case 1.3.5:
input = `@forename is Maksym and @surname is either Ivanov or Petrov 
            or @forename is Viktor and @surname is neither Sokolov nor Smirnov`             
```
output = [
    {
        forename: {
            operator: "is",
            value: "Maksym"
        },
        surname: {
            operator: "is in",
            value: ["Ivanov", "Petrov"] 
        }
    },
    {
        forename: {
            operator: "is",
            value: "Viktor"
        },
        surname: {
            operator: "is not in",
            value: ["Ivanov", "Petrov"] 
        }
    }
]     
```

## Terminal symbols:

```
AT = '@';
START_QUOTE_TERM = /["']/;
END_QUOTE_TERM = /["']/; 
IS_TERM = 'is';
EITHER_TERM = 'either';
NEITHER_TERM = 'neither';
OR_TERM = 'or';
NOR_TERM = 'nor';
AND_TERM = 'and';
NOT_TERM = 'not';
SPACE_TERM = ' ';
CHAR_TERM = /[^ @]/;
KEYWORD_TERM = [@(name|forename|surname|age)]
```

## Non terminal symbols

```
NEITHER_NOR_OPERATOR
NEITHER_OPERATOR
EITHER_OR_OPERATOR
EITHER
NOT_OPERATOR
IS_SUB_OPERATOR
IS_OPERATOR
OPERATOR
MULTI_QUOTED_WORD
VALUE
KEYWORD
OPERATOR_GROUP
OPERATOR_GROUP_OR
OPERATOR_GROUP_AND
CONJUNCTION
CRITERIA
CRITERIA_GROUP
WORD
SPACE
STATEMENT
```

## FSM logic

```
NEITHER_NOR_OPERATOR    = [SPACE | NOR_TERM | VALUE | NEITHER_NOR_OPERATOR]
NEITHER_OPERATOR        = [SPACE | NEITHER_TERM | VALUE | NEITHER_NOR_OPERATOR]
EITHER_OR_OPERATOR      = [SPACE | OR_TERM | VALUE | EITHER_OR_OPERATOR]
EITHER_OPERATOR         = [SPACE | EITHER_TERM | VALUE | EITHER_OR_OPERATOR]
NOT_OPERATOR            = [SPACE | NOT_TERM | VALUE]
IS_SUB_OPERATOR         = [NOT_OPERATOR | EITHER_OPERATOR | NEITHER_OPERATOR]
IS_OPERATOR             = [SPACE | IS_TERM | IS_SUB_OPERATOR | VALUE]
MULTI_QUOTED_WORD       = [START_QUOTE_TERM | STATEMENT | END_QUOTE_TERM]
VALUE                   = [SPACE | WORD | MULTI_QUOTED_WORD]
OPERATOR                = [IS_OPERATOR]
KEYWORD                 = [SPACE | KEYWORD_TERM]
OPERATOR_GROUP          = [KEYWORD | SPACE | OPERATOR]
OPERATOR_GROUP_AND      = [SPACE | AND_TERM | SPACE | CRITERION]
OPERATOR_GROUP_OR       = [SPACE | OR_TERM | SPACE | CRITERION]
CONJUNCTION             = [OPERATOR_GROUP_OR | OPERATOR_GROUP_AND]
CRITERION               = [OPERATOR_GROUP | CONJUNCTION]
CRITERIA                = [CRITERION | CRITERIA]
WORD                    = [CHAR_TERM | WORD]
SPACE                   = [SPACE_TERM | SPACE]
STATEMENT               = [SPACE | WORD | STATEMENT]
START                   = [STATEMENT | CRITERIA] 
```
