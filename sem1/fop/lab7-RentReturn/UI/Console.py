'''
Created on Nov 13, 2015

@author: Vlad
'''

class Console:
        def __init__(self, ctrl_car, ctrl_client, ctrl_rental):

                self.__ctrl_car = ctrl_car
                self.__ctrl_client = ctrl_client
                self.__ctrl_rental = ctrl_rental

        def show(self):
                """
                    Method displays the main menu of the application.
                """

                while True:
                        print("1. Add car")
                        print("2. Update car")
                        print("3. Delete car")
                        print("4. Add client")
                        print("5. Update client")
                        print("6. Delete client")
                        print("7. Show clients")
                        print("8. Show cars")
                        print("9. Add rental")
                        print("10. Delete rental")
                        print("11. Show rentals")

                        print("0. Exit")

                        option = input("Option: ")

                        if option == "1":
                                self.__add_car()
                        elif option == "2":
                                self.__update_cars()
                        elif option == "3":
                                self.__delete_cars()
                        elif option == "4":
                                self.__add_client()
                        elif option == "5":
                                self.__update_clients()
                        elif option == "6":
                                self.__delete_clients()
                        elif option == "7":
                                self.__show_clients()
                        elif option == "8":
                                self.__show_cars()
                        elif option == "9":
                                self.__add_rental()
                        elif option == "10":
                                self.__delete_rental()
                        elif option == "11":
                                self.__show_rentals()
                        elif option == "0":
                                break
                        else:
                                print("Invalid option, please try again!")

        def __add_car(self):
                """
                    Method displays the menu for adding a new car.
                """
                while True:
                        try:
                                id = int(input("ID: "))
                                name = input("name: ")

                                self.__ctrl_car.add(id, name)

                                break
                        except KeyError as ke:
                                print(ke)
                        except ValueError:
                                print("The ID must be an integer.")
                        except Exception as e:
                                print(e)

        def __add_client(self):
                """
                    Method displays the menu for adding a new client.
                """
                while True:
                        try:
                                id = int(input("ID: "))
                                name = input("name: ")

                                self.__ctrl_client.add(id, name)

                                break
                        except KeyError as ke:
                                print(ke)
                        except ValueError:
                                print("The ID must be an integer.")
                        except Exception as e:
                                print(e)

        def __show_cars(self):
                """
                    Method displays all cars.
                """
                print(self.__ctrl_car.get_all())

        def __show_clients(self):
                """
                    Method displays all clients.
                """
                print(self.__ctrl_client.get_all())

        def __update_cars(self):
                """
                    Method displays the menu for updating an existing car.
                """
                while True:
                        try:
                                id = int(input("ID: "))
                                name = input("name: ")

                                self.__ctrl_car.update(id, name)
                                break
                        except KeyError as ke:
                                print(ke)
                        except ValueError:
                                print("The ID must be an integer.")


        def __delete_cars(self):
                """
                    Method displays the menu for deleting an existing car.
                """
                while True:
                        try:
                                id = int(input("ID to delete: "))

                                self.__ctrl_rental.delete_by_car(id)
                                self.__ctrl_car.delete(id)
                                break
                        except KeyError as ke:
                                print(ke)
                        except ValueError:
                                print("The ID must be an integer.")


        def __update_clients(self):
                """
                    Method displays the menu for updating a client.
                """
                while True:
                        try:
                                id = int(input("ID: "))
                                name = input("name: ")

                                self.__ctrl_client.update(id, name)

                                break
                        except KeyError as ke:
                                print(ke)
                        except ValueError:
                                print("The ID must be an integer.")


        def __delete_clients(self):
                """
                    Method displays the menu for deleting an existing client.
                """
                while True:
                        try:
                                id = int(input("ID to delete: "))

                                self.__ctrl_rental.delete_by_client(id)
                                self.__ctrl_client.delete(id)

                                break
                        except KeyError as ke:
                                print(ke)
                        except ValueError:
                                print("The ID must be an integer.")

        def __add_rental(self):
                """
                    Method displays the menu for adding a new rental.
                """
                while True:
                        try:
                                id_client = int(input("ID Client: "))
                                id_car = int(input("ID Car: "))
                                price = int(input("Price: "))

                                self.__ctrl_rental.add(id_client, id_car, price)

                                break
                        except KeyError as ke:
                               print(ke)
                        except ValueError:
                               print("The IDs and price must be integers.")

        def __delete_rental(self):
                """
                    Method displays the menu for deleting an existing rental.
                """
                while True:
                        try:
                                id_client = int(input("ID of the client: "))
                                id_car = int(input("ID of the car: "))

                                self.__ctrl_rental.delete(id_client, id_car)

                                break
                        except KeyError as ke:
                                print(ke)

                        except ValueError:
                                print("The IDs must be integers.")

        def __show_rentals(self):
                """
                    Method displays all existing rentals.
                """
                print(self.__ctrl_rental.get_all())
