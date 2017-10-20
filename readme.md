[![Build Status](https://travis-ci.org/maximoleinyk/atBox.svg?branch=master)](https://travis-ci.org/maximoleinyk/atBox)

# Demo
![](https://github.com/maximoleinyk/atBox/blob/master/resources/demo.gif)

# Specification
             
## Is
        
### Case 1.1.1:
input = `find a person whose @name is Maksym`
```
output = {
    "field": "name",
    "operator": "==",
    "value": "Maksym"
}
```       

### Case 1.1.2:
input = `find person whose @name is "Maksym"`

```
output = {
    "field": "name",
    "operator": "==",
    "value": "Maksym"
}
```
   
### Case 1.1.3:
input = `find a person whose @name is "Maksym Oliinyk"`
```
output = {
    "field": "name",
    "operator": "==",
    "value": "Maksym Oliinyk"
}
```
    
### Case 1.1.4:
input = `@name is "Maksym Oliinyk"`    
```
output = {
    "field": "name",
    "operator": "==",
    "value": "Maksym Oliinyk"
}  
```
    
### Case 1.1.5:
input = `find a person whose name Maksym`    
```
output = null
```

### Case 1.1.6:
input = `find a person whose @ name is Maksym`    
```
output = null   
```

### Case 1.1.7:
input = `find@name is Maksym`    
```
output = null
```

### Case 1.1.8:
input = `@@@name is Max`    
```
output = null
```

### Case 1.1.9:
input = `@nonexistingfield is Max`    
```
output = [
]        
```

### Case 1.1.10:
input = `@name could be or is Maksym Oliinyk`    
```
output = null 
```
    
## Is not

### Case 1.2.1:
input = `@name is not Maksym`    
```
output = {
    "field": "name",
    "operator": "!=",
    "value": "Maksym"
}
```

### Case 1.2.2:
input = `@name is something not something Maksym`    
```
output = {
    "field": "name",
    "operator": "==",
    "value": "Maksym"
} 
```

### Case 1.2.3:
input = `find a person whose @name is not Maksym Oliinyk`    
```
output = {
    "field": "name",
    "operator": "!=",
    "value": "Maksym"
}    
```

### Case 1.2.4:
input = `@name is not "Maksym Oliinyk"`    
```
output = {
    "field": "name",
    "operator": "!=",
    "value": "Maksym Oliinyk"
}  
```

## Either or / Neither nor

### Case 1.3.1:
input = `@forename is either Maksym or Viktor`    
```
output = {
    "field": "forename",
    "operator": "in",
    "value": [
        "\"Maksym\"",
        "\"Viktor\""
    ]
} 
```

### Case 1.3.2:
input = `@forename is neither Maksym nor Viktor`    
```
output = {
     "field": "forename",
     "operator": "not in",
     "value": [
         "\"Maksym\"",
         "\"Viktor\""
     ]
}    
```

## And

### Case 1.4.1:
input = `@forename is Maksym and @surname Oliinyk and @age is 26`    
```
output = {
    "and": [
        {
            "and": [
                {
                    "field": "forename",
                    "operator": "==",
                    "value": "Maksym"
                },
                null
            ]
        },
        {
            "field": "age",
            "operator": "==",
            "value": "26"
        }
    ]
}
```

### Case 1.4.2:
input = `@age is 26 and @name is "Maksym Oliinyk"`    
```
output = {
    "and": [
        {
            "field": "age",
            "operator": "==",
            "value": "26"
        },
        {
            "field": "name",
            "operator": "==",
            "value": "Maksym Oliinyk"
        }
    ]
}  
```

### Case 1.4.3:
input = `@forename is Maksym and @surname is either Ivanov or Petrov`    
```
output = {
    "and": [
        {
            "field": "forename",
            "operator": "==",
            "value": "Maksym"
        },
        {
            "field": "surname",
            "operator": "in",
            "value": [
                "\"Ivanov\"",
                "\"Petrov\""
            ]
        }
    ]
}   
```

## Or

### Case 1.5.1:
input = `@forename is Maksym or @surname Oliinyk`    
```
output = {
    "or": [
        {
            "field": "forename",
            "operator": "==",
            "value": "Maksym"
        },
        null
    ]
}   
```

### Case 1.5.2:
input = `@forename is Maksym and @surname is either Ivanov or Petrov 
            or @forename is Viktor and @surname is neither Sokolov nor Smirnov`             
```
output = {
    "or": [
        {
            "and": [
                {
                    "field": "forename",
                    "operator": "==",
                    "value": "Maksym"
                },
                {
                    "field": "surname",
                    "operator": "in",
                    "value": [
                        "\"Ivanov\"",
                        "\"Petrov\""
                    ]
                }
            ]
        },
        {
            "and": [
                {
                    "field": "forename",
                    "operator": "==",
                    "value": "Viktor"
                },
                {
                    "field": "surname",
                    "operator": "not in",
                    "value": [
                        "\"Sokolov\"",
                        "\"Smirnov\""
                    ]
                }
            ]
        }
    ]
}  
```

## In

### Case 1.6.1:
input = `@forename is in (Maksym, Viktor)`    
```
output = {
    "field": "forename",
    "operator": "in",
    "value": [
        "\"Maksym\"",
        "\"Viktor\""
    ]
}    
```

### Case 1.6.2:
input = `@forename is in ()`    
```
output = {
    "field": "forename",
    "operator": "in",
    "value": []
} 
```

### Case 1.6.3:
input = `@forename is in`    
```
output = null   
```

### Case 1.7.1:
input = `@forename is in (Maksym, Viktor) or @forename is not in (Alex, Julia)`    
```
output = 
    "or": [
        {
            "field": "forename",
            "operator": "in",
            "value": [
                "\"Maksym\"",
                "\"Viktor\""
            ]
        },
        {
            "field": "forename",
            "operator": "not in",
            "value": [
                "\"Alex\"",
                "\"Julia\""
            ]
        }
    ]
}
```

### Case 1.7.2:
input = `((@name is either Max or Joe) or (@surname is neither Oliinyk nor Doe)) 
            and ((@age is not 27) or (@forename is Oliinyk)) 
                or @surname is not Smirnov`
```
output = {
    "or": [
        {
            "and": [
                {
                    "or": [
                        {
                            "field": "name",
                            "operator": "in",
                            "value": [
                                "Max",
                                "Joe"
                            ]
                        },
                        {
                            "field": "surname",
                            "operator": "not in",
                            "value": [
                                "Oliinyk",
                                "Doe"
                            ]
                        }
                    ]
                },
                {
                    "or": [
                        {
                            "field": "age",
                            "operator": "!=",
                            "value": "27"
                        },
                        {
                            "field": "forename",
                            "operator": "==",
                            "value": "Oliinyk"
                        }
                    ]
                }
            ]
        },
        {
            "field": "surname",
            "operator": "!=",
            "value": "Smirnov"
        }
    ]
}
```