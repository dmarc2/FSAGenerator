from tkinter import *
import sys

screenW = 301
screenH = 400
screenCenter = 151  #math.ceil(sW) 

class FSA:
    def __init__(self,filename) -> None:
        self.initFSA(filename)
    
    def initFSA(self,filename) -> None:
        fsaString = None
        with open(filename) as f:
            fsaString = f.readlines()
        fsaString = str(fsaString).strip('\'[];').split(';')
        self.numOfStates = int(fsaString[0])
        self.alphabet = fsaString[1].split(',')
        self.alphabet.sort()
        self.transitions = fsaString[2].split(',')
        self.initTransMatrix(fsaString[2].split(','))
        self.startState = int(fsaString[3])
        self.acceptStates = fsaString[4].split(',')
    
    def initTransMatrix(self,transitions):
        self.matrix = [['-' for x in range(len(self.alphabet))] for y in range(self.numOfStates)] 
        for i in range(len(transitions)):
            currIter = transitions[i].strip('()').split(':')
            self.matrix[int(currIter[0])][self.binarySearch(self.alphabet,currIter[2])] = currIter[1]

    def binarySearch(self,l,alpha) -> int:
        low = 0
        high = len(l)-1
        while (low <= high):
            mid = low + (high - low) // 2
            if (alpha == l[mid]):
                return mid
            elif (alpha > l[mid]):
                low = mid + 1
            else:
                high = mid - 1
        return -1

    def testInput(self, theString) -> bool:
        currentState = self.startState
        for i in range(len(theString)):
            wordAlphaIndex = self.binarySearch(self.alphabet, theString[i])
            if wordAlphaIndex == -1:
                currentState = -1
                break

            nextState = self.matrix[currentState][wordAlphaIndex]
            if nextState == '-':
                currentState = -1
                break

            currentState = int(nextState)
        return self.isAcceptState(currentState)

    def isAcceptState(self,state):
        for i in range(len(self.acceptStates)):
            if str(state) == self.acceptStates[i]:
                return True
        return False

    def getLoopAlpha(self,state:int):
        for i in range(len(self.alphabet)):
            if self.matrix[state][i] == str(state):
                return self.alphabet[i]
        return None

    def draw(self,canvas:Canvas) -> None:
        x = 136
        y = 30
        nodeW = 30
        nodeH = 30
        for state in range(self.numOfStates):
            if state == 0:
                canvas.create_line(screenCenter,10,screenCenter,30,arrow="last")#Start state arrow

            if self.hasArc(state):
                canvas.create_oval(x+nodeW/2,y,x+nodeW+nodeW/2,y+nodeH)
                canvas.create_text(screenCenter+10,y-1,text='L')
                canvas.create_text(screenCenter+nodeW+5,y+nodeH/2,text=self.getLoopAlpha(state))

            if self.isAcceptState(state):
                canvas.create_oval(x-3,y-3,x+nodeW+3,y+nodeH+3)

            canvas.create_oval(x,y,x+nodeW,y+nodeH,fill="lightgray")
            canvas.create_text(screenCenter,y+nodeH/2,text=state)
            #drawTransitions
            for j in range(len(self.alphabet)):
                nextState = self.matrix[state][j]
                if nextState == '-' or nextState == str(state):
                    continue
                distance = int(nextState) - state
                if distance == 1:
                    canvas.create_text(screenCenter+5,y+nodeH+10,text=self.alphabet[j])
                    canvas.create_line(screenCenter,y+nodeH,screenCenter,y+nodeH*2,arrow="last")
                elif distance > 1:
                    offset = 5
                    canvas.create_text(x-25*(abs(distance)-1)-offset*5,y+nodeH*abs(distance),text=self.alphabet[j])
                    canvas.create_line((x,y+nodeH/2,x-nodeW*(abs(distance)-1)-offset,y+nodeH/2,x-nodeW*(abs(distance)-1)-offset,y+nodeH*2*(abs(distance))+15,x,y+nodeH*2*(abs(distance))+15),arrow="last")
                else:
                    canvas.create_text(x-25*(abs(distance)-1),y-nodeH*abs(distance),text=self.alphabet[j])
                    canvas.create_line((x,y+nodeH/2,x-nodeW*(abs(distance)-1),y+nodeH/2,x-nodeW*(abs(distance)-1),y-nodeH*2*(abs(distance))+15,x,y-nodeH*2*(abs(distance))+15),arrow="last")
            y += nodeH*2

    def hasArc(self,state):
        for i in range(len(self.alphabet)):
            if self.matrix[state][i] == str(state):
                return True
        return False

    def generateLispCode(self):
        with open("part2.lsp",'w') as fp:
            fp.write(
                f"(defun demo()\n\
  (setq fsa '((start ({self.startState})) (accept ({' '.join(self.acceptStates)}))\n\
    (transitions ({' '.join(self.transitions).replace(':', ' ')}))))\n\
  (setq state (car (assoc 'start fsa)))\n\
  (readFile)\n\
  (princ \"Processing the string \")\n\
  (print theString)\n\
  (testInput theString (assoc 'transitions fsa))\n\
)")
            fp.write("\n\n(defun testInput(theString l)\n\
  	(setq state (getNextState state (car theString) l))\n\
	(if (eq state NIL) (princ \"reject\")\n\
	 (if (eq (cdr theString) NIL) (if (eq (isAccept state (assoc 'accept fsa)) T) (princ \"accept\") (princ \"reject\")) (testInput (cdr theString) l)))\n\
)\n\
\n\
(defun getNextState(state X L)\n\
    (COND ((NULL L) (princ \"invalid char \") (princ X) (princ \" in state \") (print state) NIL)\n\
        ((ATOM L)	 NIL)\n\
        ((and  (EQUAL X (CADDAR L)) (EQUAL state (CAAR L)) (CADAR L)))\n\
        (T  (getNextState state X (CDR L)))\n\
    )\n\
)\n\
\n\
(defun isAccept (state l)\n\
    (COND ((NULL l)	 NIL)\n\
        ((ATOM l)	 NIL)\n\
        ((EQUAL state (CAR l)) T)\n\
        (T  (isAccept state (CDR l)))\n\
    )\n\
)\n\
\n\
(defun assoc (p l)\n\
    (COND ((null l)	 nil)\n\
        ((atom l)	 nil)\n\
        ((equal p (caar l)) (cadar l))\n\
        (t (assoc p (cdr l)))\n\
    )\n\
)\n\
\n\
(defun readFile()\n\
	(setq fp (open \"theString.txt\" :direction :input))\n\
	(setq theString (read fp \"done\"))\n\
	(close fp)\n\
)\n\
")
        fp.close()
    
def main():
    if len(sys.argv) != 2:
        #print("Usage: fsa.py fsa.txt inputFile.txt")
        print("Usage: fsa.py fsa.txt")
        exit(1)

    fa = FSA(sys.argv[1])
    
    fa.generateLispCode()

    #Root window
    root = Tk()
    canvas = Canvas(root, width=301, height=400, background="lightgray")
    fa.draw(canvas) 
    canvas.pack()
    root.title("FSA")
    root.mainloop()

if __name__ == "__main__":
    main()