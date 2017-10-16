# Demo
![alt text](https://github.com/maximoleinyk/atBox/master/resources/demo.gif)

# Specification
             
## Is
        
### Case 1.1.1:
input = `find a person whose @name is Maksym`
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
input = `find a person whose @name is "Maksym Oliinyk"`
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
input = `find a person whose name Maksym`    
```
output = [
]        
```

### Case 1.1.6:
input = `find a person whose @ name is Maksym`    
```
output = [
]        
```

### Case 1.1.7:
input = `find@name is Maksym`    
```
output = [
]       
```

### Case 1.1.8:
input = `@@@name is Max`    
```
output = [
]        
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
output = [
]        
```
    
## Is not

### Case 1.2.1:
input = `@name is not Maksym`    
```
output = [
    {
        "name": {
            operator: "not"
            value: "Maksym"
        }
    }    
]    
```

### Case 1.2.2:
input = `@name is something not something Maksym`    
```
output = [
    {
        "name": {
            operator: "not"
            value: "Maksym"
        }
    }    
]    
```

### Case 1.2.3:
input = `find a person whose @name is not Maksym Oliinyk`    
```
output = [
    {
        "name": {
            operator: "not"
            value: "Maksym"
        }
    }
]        
```

### Case 1.2.4:
input = `@name is not "Maksym Oliinyk"`    
```
output = [
    {
        "name": {
            operator: "not"
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
            operator: "in",
            value: [
                "Maksym", 
                "Viktor"
            ]
        }
    }
]     
```

### Case 1.3.2:
input = `@forename is neither Maksym nor Viktor`    
```
output = [
    {
        forename: {
            operator: "not in",
            value: ["Maksym", "Viktor"]
        }
    }
]     
```

## And

### Case 1.4.1:
input = `@forename is Maksym and @surname Oliinyk and @age is 26`    
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
        },
        age: {
            operator: "is",
            value: 26
        }
    }
]     
```

### Case 1.4.2:
input = `@age is 26 and @name is "Maksym Oliinyk"`    
```
output = [
    {
        age: {
            operator: "is",
            value: 26
        },
        name: {
            operator: "is",
            value: "Maksym Oliinyk"
        }
    }
]     
```

### Case 1.4.3:
input = `@forename is Maksym and @surname is either Ivanov or Petrov`    
```
output = [
    {
        forename: {
            operator: "is",
            value: "Maksym"
        },
        surname: {
            operator: "in",
            value: ["Ivanov", "Petrov"] 
        }
    }
]     
```

## Or

### Case 1.5.1:
input = `@forename is Maksym or @surname Oliinyk`    
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
        },
        age: {
            operator: "is",
            value: 26
        }
    }
]     
```

### Case 1.5.2:
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
            operator: "in",
            value: ["Ivanov", "Petrov"] 
        }
    },
    {
        forename: {
            operator: "is",
            value: "Viktor"
        },
        surname: {
            operator: "not in",
            value: ["Ivanov", "Petrov"] 
        }
    }
]     
```

## In

### Case 1.6.1:
input = `@forename is in (Maksym, Viktor)`    
```
output = [
    {
        forename: {
            operator: "in",
            value: ["Maksym", "Viktor"]
        }
    }
]     
```

### Case 1.6.2:
input = `@forename is in ()`    
```
output = [
    {
        forename: {
            operator: "in",
            value: []
        }
    }
]     
```

### Case 1.6.3:
input = `@forename is in`    
```
output = []     
```

### Case 1.7.1:
input = `@forename is in (Maksym, Viktor) or @forename is not in (Alex, Julia)`    
```
output = [
    {
        forename: {
            operator: "in",
            value: ["Maksym", "Viktor"]
        }
    },
    {
        forename: {
            operator: "not in",
            value: ["Alex", "Julia"]
        }
    }
]     
```