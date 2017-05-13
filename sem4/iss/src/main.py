#!/usr/bin/python3

import itertools
import pynder

from flask import Flask, request, session, escape, render_template, abort, redirect, url_for

from PIL import Image

import re
import robobrowser
import urllib.request
import shutil
import os.path
import getpass

from subprocess import call


app = Flask(__name__)
app.secret_key = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'


FBID = "xxxxxxxxxxxxxxx"
FBUSER = "xxxxxxxxxxxxxxxx@gmail.com"
FBPASS = "xxxxxxxxxxxxx"
PHOTO_DIR = 'tmp/'

def get_filename_from_url(url):
    return url.split("/")[-1]

def download_photo(url):
    filename = get_filename_from_url(url)
    filepath = os.path.join(PHOTO_DIR, filename)

    with urllib.request.urlopen(url) as response, open(filepath, 'wb') as out_file:
        shutil.copyfileobj(response, out_file)

def create_image_objects(files):
    images = []
    for filename in files:
        images.append(Image.open(filename))
    return images

def show_photos(images):
    for image in images:
        image.show()

def close_photos(images):
    for image in images:
        image.close()

def show_photos_with_feh(files):
    command = "feh -F -d " + ' '.join(files)
    call(command, shell=True)

def get_access_token(email, password):
    MOBILE_USER_AGENT = "Mozilla/5.0 (Linux; U; en-gb; KFTHWI Build/JDQ39) AppleWebKit/535.19 (KHTML, like Gecko) Silk/3.16 Safari/535.19"
    FB_AUTH = "https://www.facebook.com/v2.6/dialog/oauth?redirect_uri=fb464891386855067%3A%2F%2Fauthorize%2F&display=touch&state=%7B%22challenge%22%3A%22IUUkEUqIGud332lfu%252BMJhxL4Wlc%253D%22%2C%220_auth_logger_id%22%3A%2230F06532-A1B9-4B10-BB28-B29956C71AB1%22%2C%22com.facebook.sdk_client_state%22%3Atrue%2C%223_method%22%3A%22sfvc_auth%22%7D&scope=user_birthday%2Cuser_photos%2Cuser_education_history%2Cemail%2Cuser_relationship_details%2Cuser_friends%2Cuser_work_history%2Cuser_likes&response_type=token%2Csigned_request&default_audience=friends&return_scopes=true&auth_type=rerequest&client_id=464891386855067&ret=login&sdk=ios&logger_id=30F06532-A1B9-4B10-BB28-B29956C71AB1&ext=1470840777&hash=AeZqkIcf-NEW6vBd"
    s = robobrowser.RoboBrowser(user_agent=MOBILE_USER_AGENT, parser="lxml")
    s.open(FB_AUTH)
    ##submit login form##
    f = s.get_form()
    f["pass"] = password
    f["email"] = email
    s.submit_form(f)
    ##click the 'ok' button on the dialog informing you that you have already authenticated with the Tinder app##
    f = s.get_form()
    s.submit_form(f, submit=f.submit_fields['__CONFIRM__'])
    ##get access token from the html response##
    access_token = re.search(r"access_token=([\w\d]+)", s.response.content.decode()).groups()[0]
    #print  s.response.content.decode()
    return access_token

def create_pynder_session(FBID, FBTOKEN):
    session = pynder.Session(facebook_id=FBID, facebook_token=FBTOKEN)
    return session

def vote_once(session):
    print("Remaining likes: %i" % session.likes_remaining)

    try:
        person = list(itertools.islice(session.nearby_users(), 0, 1))[0]
    except:
        print("Could not get any nearby persons.")
        exit(1)

    print("ID: %s" % person.id)
    print("Name: %s" % person.name)
    print("Birthdate: %s (%i years old)" % (str(person.birth_date).split()[0], person.age))
    print("Dist: %.0f km" % person.distance_km)
    if person.instagram_username is not None:
        print("Instagram: %s" % person.instagram_username)

    if person.bio.strip() not in [None, '']:
        print("Bio: %s" % person.bio)

    if person.schools != []:
        sep = ' ' if len(person.schools) == 1 else '\n'
        print("Schools:%s%s" % (sep, sep.join(person.schools)))

    if person.jobs != []:
        sep = ' ' if len(person.jobs) == 1 else '\n'
        print("Jobs:%s%s" % (sep, sep.join(person.jobs)))

    for photo_url in person.photos:
        try:
            download_photo(photo_url)
        except:
            print("Could not download photo. Skipping.")

    photo_filenames = map(lambda url: os.path.join(PHOTO_DIR, get_filename_from_url(url)), person.photos)
    # images = create_image_objects(photo_filenames)

    # show_photos(images)
    show_photos_with_feh(photo_filenames)

    match = None
    while True:
        print("Like? (y/n/s): ", end='')
        decision = input()

        if decision == 'y':
            match = person.like()
            break
        elif decision == 'n':
            match = person.dislike()
            break
        elif decision == 's':
            try:
                match = person.superlike()
            except Exception as e:
                print(e)
                continue
            break

    print("Match: %r" % match)

    # close_photos(images)
    print()

def show(x):
    print("\n".join(dir(x)))

def show_messages(session, index):
    print('\n'.join(map(lambda x: x.__repr__(), list(itertools.islice(session.matches(), 0, 100))[index].messages)))

def send_message(session, index, message):
    print(list(itertools.islice(session.matches(), 0, 100))[index].message(message))

def read_credentials():
    FBUSER = input("Email: ")
    FBPASS = getpass.getpass("Pass: ")

    return (FBUSER, FBPASS)


@app.route("/")
def index():
    if 'username' in session:
        return "logged in as %s. <a href=\"%s\">log out</a>" % (escape(session['username']), url_for("logout"))
    return redirect(url_for('login'))

@app.route('/login', methods=['GET', 'POST'])
def login():
    if request.method == 'POST':
        try:
            username = request.form['username']
            password = request.form['password']

            access_token = get_access_token(username, password)
            print("Access token is %s" % access_token)

            session['username'] = username
            session['access_token'] = access_token

            return "Logged in as %s with the following access token:<br>%s<br><a href=\"%s\">Go back</a>" % (username, access_token, url_for("index"))

        except Exception as e:
            return "Could not get access token. Reason:<br>%s" % e
    else:
        if 'username' in session:
            return redirect(url_for('index'))
        return render_template('login.html')

@app.route('/logout')
def logout():
    # remove the username from the session if it's there
    session.pop('username', None)
    return redirect(url_for('index'))

@app.errorhandler(404)
def page_not_found(error):
    return "page not found", 404

def main():
    app.run()

if __name__=='__main__':
    main()
