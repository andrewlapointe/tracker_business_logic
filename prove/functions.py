"""
Course: CSE 251, week 14
File: functions.py
Author: <your name>

Instructions:

Depth First Search
https://www.youtube.com/watch?v=9RHO6jU--GU

Breadth First Search
https://www.youtube.com/watch?v=86g8jAQug04


Requesting a family from the server:
family_id = 6128784944
request = Request_thread(f'{TOP_API_URL}/family/{family_id}')
request.start()
request.join()

Example JSON returned from the server
{
    'id': 6128784944, 
    'husband_id': 2367673859,        # use with the Person API
    'wife_id': 2373686152,           # use with the Person API
    'children': [2380738417, 2185423094, 2192483455]    # use with the Person API
}

Requesting an individual from the server:
person_id = 2373686152
request = Request_thread(f'{TOP_API_URL}/person/{person_id}')
request.start()
request.join()

Example JSON returned from the server
{
    'id': 2373686152, 
    'name': 'Stella', 
    'birth': '9-3-1846', 
    'parent_id': 5428641880,   # use with the Family API
    'family_id': 6128784944    # use with the Family API
}

You will lose 10% if you don't detail your part 1 and part 2 code below

Describe how to speed up part 1

<Add your comments here>


Describe how to speed up part 2

<Add your comments here>


Extra (Optional) 10% Bonus to speed up part 3

<Add your comments here>

"""
from common import *
import queue

# -----------------------------------------------------------------------------
def depth_fs_pedigree(family_id, tree):
    # KEEP this function even if you don't implement it
    # TODO - implement Depth first retrieval
    # TODO - Printing out people and families that are retrieved from the server will help debugging

    visited_families = set()

    def dfs(family_id):
        if not family_id or family_id in visited_families:
            return
        visited_families.add(family_id)

        # Request family details
        family_request = Request_thread(f'{TOP_API_URL}/family/{family_id}')
        family_request.start()
        family_request.join()
        family_data = family_request.get_response()

        if not family_data:
            print(f"Family ID {family_id} not found. Skipping...")
            return

        family = Family(family_data)
        tree.add_family(family)

        # Request family members
        members = [family.get_husband(), family.get_wife()] + family.get_children()
        threads = []
        for member_id in members:
            if member_id and not tree.does_person_exist(member_id):
                thread = Request_thread(f'{TOP_API_URL}/person/{member_id}')
                threads.append(thread)
                thread.start()

        for thread in threads:
            thread.join()
            person_data = thread.get_response()
            if person_data:
                tree.add_person(Person(person_data))

        # Assign the current family ID to children with missing family relationships
        for child_id in family.get_children():
            child = tree.get_person(child_id)
            if child:
                next_family_id = child.get_familyid() or family_id  # Use current family ID if missing
                print(f"Child ID: {child_id}, Next Family ID: {next_family_id}")
                if next_family_id:
                    dfs(next_family_id)
                else:
                    print(f"Skipping child ID {child_id}: No family or parent relationship found.")

    dfs(family_id)

# -----------------------------------------------------------------------------
def breadth_fs_pedigree(family_id, tree):
    # KEEP this function even if you don't implement it
    # TODO - implement breadth first retrieval
    # TODO - Printing out people and families that are retrieved from the server will help debugging

    family_queue = queue.Queue()
    visited_families = set()

    family_queue.put(family_id)
    visited_families.add(family_id)

    while not family_queue.empty():
        current_family_id = family_queue.get()

        # Request family details
        family_request = Request_thread(f'{TOP_API_URL}/family/{current_family_id}')
        family_request.start()
        family_request.join()
        family_data = family_request.get_response()

        if not family_data:
            print(f"Family ID {current_family_id} not found. Skipping...")
            continue

        family = Family(family_data)
        if not tree.does_family_exist(current_family_id):
            tree.add_family(family)

        # Request family members concurrently
        members = [family.get_husband(), family.get_wife()] + family.get_children()
        threads = []
        for member_id in members:
            if member_id and not tree.does_person_exist(member_id):
                thread = Request_thread(f'{TOP_API_URL}/person/{member_id}')
                threads.append(thread)
                thread.start()

        for thread in threads:
            thread.join()
            person_data = thread.get_response()
            if person_data and not tree.does_person_exist(person_data['id']):
                tree.add_person(Person(person_data))

        # Enqueue children's families
        for child_id in family.get_children():
            child = tree.get_person(child_id)
            if child and child.get_familyid() and child.get_familyid() not in visited_families:
                family_queue.put(child.get_familyid())
                visited_families.add(child.get_familyid())

# -----------------------------------------------------------------------------
def breadth_fs_pedigree_limit5(family_id, tree):
    # KEEP this function even if you don't implement it
    # TODO - implement breadth first retrieval
    #      - Limit number of concurrent connections to the FS server to 5
    # TODO - Printing out people and families that are retrieved from the server will help debugging

    family_queue = queue.Queue()
    visited_families = set()

    family_queue.put(family_id)
    visited_families.add(family_id)

    semaphore = threading.Semaphore(5)

    while not family_queue.empty():
        current_family_id = family_queue.get()

        # Request family details
        family_request = Request_thread(f'{TOP_API_URL}/family/{current_family_id}')
        family_request.start()
        family_request.join()
        family_data = family_request.get_response()

        if not family_data:
            continue

        family = Family(family_data)
        tree.add_family(family)

        # Request family members concurrently with thread limit
        members = [family.get_husband(), family.get_wife()] + family.get_children()
        threads = []
        for member_id in members:
            if member_id and not tree.does_person_exist(member_id):
                semaphore.acquire()
                try:
                    thread = Request_thread(f'{TOP_API_URL}/person/{member_id}')
                    threads.append(thread)
                    thread.start()
                finally:
                    semaphore.release()

        for thread in threads:
            thread.join()
            person_data = thread.get_response()
            if person_data:
                tree.add_person(Person(person_data))

        # Enqueue children's families
        for child_id in family.get_children():
            child = tree.get_person(child_id)
            if child and child.get_familyid() and child.get_familyid() not in visited_families:
                family_queue.put(child.get_familyid())
                visited_families.add(child.get_familyid())