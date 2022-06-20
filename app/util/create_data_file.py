from datetime import date
from datetime import timedelta

def main():
    today = date.today()

    f= open("../src/resources/DEMO_DATA.DAT","w+")
 
    for i in range(5):
        current_date = today - timedelta(days=i)
        for k in range(10):
            text = current_date.strftime("%Y%m%d%d") + "A00" + str(k)
            if k != 9 or i != 4:
                text = text + "\n"
            f.write(text)
    f.close()
    
if __name__== "__main__":
  main()