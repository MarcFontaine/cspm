-- this is exprimental
--
require 'strict'
local CSPM = require 'cspmPrelude'

function main()
    print "start"
    test1 ()
    if #arg < 4 then return "not enough arguments" end
    test2 (arg[1],arg[2])
    print "finished"
end

function test1 ()
    print("eval an expressions ", CSPM.eval("3+3"):toString())
    print(CSPM.help)
end

function test2 (spec,entry)
    print ("Specification : ", spec)
    print ("Init          : ", entry)

    print "Loading Specification"
    local proc = CSPM.eval(entry,spec)

    print("Init-Value :", proc:toString())

    trace (10,proc)
end

function trace(maxSteps,currentState)
    for step =1, maxSteps do
        print ("Step", step, ":")
        print ("State =", currentState:toString())
        local transitions = currentState:transitions()
        if #transitions == 0 then break end
        showTransitionTable(transitions)
        currentState = transitions[1].nextState
    end
end

function showTransitionTable (transitions)
    print ("Number of Transitions :", #transitions )
    for i,trans in ipairs(transitions) do
       print ("", i, ":", trans.event)
    end
 end

test1 ()
test2 ('funBench.csp','P2')
