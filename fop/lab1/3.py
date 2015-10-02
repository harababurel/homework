import datetime

day, month, year = map(int, input().split())

now = datetime.datetime.today()
birthdate = datetime.datetime(year, month, day)

print((now - birthdate).days)
