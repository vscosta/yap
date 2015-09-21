--========================================================================== --
--  Tables                                                                   --
--========================================================================== --
DROP TABLE IF EXISTS person                    CASCADE;
DROP TABLE IF EXISTS login                     CASCADE;
DROP TABLE IF EXISTS project                   CASCADE;
DROP TABLE IF EXISTS team_member_project_assn  CASCADE;
DROP TABLE IF EXISTS project_status_type       CASCADE;
DROP TABLE IF EXISTS person_with_lock          CASCADE;
DROP TABLE IF EXISTS related_project_assn      CASCADE;
DROP TABLE IF EXISTS milestone                 CASCADE;
DROP TABLE IF EXISTS address                   CASCADE;

CREATE TABLE person (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    first_name VARCHAR(50) NOT NULL,
    last_name VARCHAR(50) NOT NULL
) ;
    CREATE INDEX IDX_person_1 ON person(last_name);

CREATE TABLE login (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    person_id BIGSERIAL NOT NULL references person(id) UNIQUE,
    username VARCHAR(20) NOT NULL UNIQUE,
    password VARCHAR(20),
    is_enabled INTEGER NOT NULL
) ;

CREATE TABLE project_status_type (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    name VARCHAR(50) NOT NULL UNIQUE,
    description TEXT,
    guidelines TEXT
) ;

CREATE TABLE project (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    project_status_type_id INTEGER NOT NULL REFERENCES project_status_type(id),
    manager_person_id INTEGER references person(id),
    name VARCHAR(100) NOT NULL,
    description TEXT,
    start_date DATE,
    end_date DATE,
    budget DECIMAL,
    spent DECIMAL
) ;
   CREATE INDEX IDX_project_1 ON project (project_status_type_id);
   CREATE INDEX IDX_project_2 ON project(manager_person_id);

CREATE TABLE team_member_project_assn (
    person_id INTEGER NOT NULL references person(id),
    project_id INTEGER NOT NULL references project(id)
) ;
   CREATE INDEX IDX_teammemberprojectassn_2 on team_member_project_assn (project_id);

CREATE TABLE person_with_lock (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    first_name VARCHAR(50) NOT NULL,
    last_name VARCHAR(50) NOT NULL,
    sys_timestamp TIMESTAMP
) ;

CREATE TABLE related_project_assn (
  project_id INTEGER NOT NULL references project(id),
  child_project_id INTEGER NOT NULL references project(id)
) ;
   CREATE INDEX IDX_relatedprojectassn_2 ON related_project_assn(child_project_id);

CREATE TABLE milestone (
  id BIGSERIAL NOT NULL PRIMARY KEY,
  project_id INTEGER NOT NULL references project(id),
  name VARCHAR(50) NOT NULL
) ;
   CREATE INDEX IDX_milestoneproj_1 ON milestone(project_id);

CREATE TABLE address (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    person_id INTEGER references person(id),
    street VARCHAR(100) NOT NULL,
    city VARCHAR(100)
) ;
   CREATE INDEX IDX_address_1 ON address (person_id);


--========================================================================== --
--  Type Data                                                                --
--========================================================================== --

INSERT INTO project_status_type (name, description, guidelines) VALUES ('Open', 'The project is currently active', 'All projects that we are working on should be in this state');
INSERT INTO project_status_type (name, description, guidelines) VALUES ('Cancelled', 'The project has been canned', null);
INSERT INTO project_status_type (name, description, guidelines) VALUES ('Completed', 'The project has been completed successfully', 'Celebrate successes!');


--========================================================================== --
--  Example Data                                                             --
--========================================================================== --

INSERT INTO person(first_name, last_name) VALUES ('John', 'Doe');
INSERT INTO person(first_name, last_name) VALUES ('Kendall', 'Public');
INSERT INTO person(first_name, last_name) VALUES ('Ben', 'Robinson');
INSERT INTO person(first_name, last_name) VALUES ('Mike', 'Ho');
INSERT INTO person(first_name, last_name) VALUES ('Alex', 'Smith'); 
INSERT INTO person(first_name, last_name) VALUES ('Wendy', 'Smith');
INSERT INTO person(first_name, last_name) VALUES ('Karen', 'Wolfe');
INSERT INTO person(first_name, last_name) VALUES ('Samantha', 'Jones');
INSERT INTO person(first_name, last_name) VALUES ('Linda', 'Brady');
INSERT INTO person(first_name, last_name) VALUES ('Jennifer', 'Smith');
INSERT INTO person(first_name, last_name) VALUES ('Brett', 'Carlisle');
INSERT INTO person(first_name, last_name) VALUES ('Jacob', 'Pratt');

INSERT INTO person_with_lock(first_name, last_name) VALUES ('John', 'Doe');
INSERT INTO person_with_lock(first_name, last_name) VALUES ('Kendall', 'Public');
INSERT INTO person_with_lock(first_name, last_name) VALUES ('Ben', 'Robinson');
INSERT INTO person_with_lock(first_name, last_name) VALUES ('Mike', 'Ho');
INSERT INTO person_with_lock(first_name, last_name) VALUES ('Alfred', 'Newman');  
INSERT INTO person_with_lock(first_name, last_name) VALUES ('Wendy', 'Johnson');
INSERT INTO person_with_lock(first_name, last_name) VALUES ('Karen', 'Wolfe');
INSERT INTO person_with_lock(first_name, last_name) VALUES ('Samantha', 'Jones');
INSERT INTO person_with_lock(first_name, last_name) VALUES ('Linda', 'Brady');
INSERT INTO person_with_lock(first_name, last_name) VALUES ('Jennifer', 'Smith');
INSERT INTO person_with_lock(first_name, last_name) VALUES ('Brett', 'Carlisle');
INSERT INTO person_with_lock(first_name, last_name) VALUES ('Jacob', 'Pratt');

INSERT INTO login(person_id, username, password, is_enabled) VALUES (1, 'jdoe', 'p@$$.w0rd', 0);
INSERT INTO login(person_id, username, password, is_enabled) VALUES (3, 'brobinson', 'p@$$.w0rd', 1);
INSERT INTO login(person_id, username, password, is_enabled) VALUES (4, 'mho', 'p@$$.w0rd', 1);
INSERT INTO login(person_id, username, password, is_enabled) VALUES (7, 'kwolfe', 'p@$$.w0rd', 0);

INSERT INTO project(project_status_type_id, manager_person_id, name, description, start_date, end_date, budget, spent) VALUES
  (3, 7, 'ACME Website Redesign', 'The redesign of the main website for ACME Incorporated', '2004-03-01', '2004-07-01', '9560.25', '10250.75');
INSERT INTO project(project_status_type_id, manager_person_id, name, description, start_date, end_date, budget, spent) VALUES
  (1, 4, 'State College HR System', 'Implementation of a back-office Human Resources system for State College', '2006-02-15', NULL, '80500.00', '73200.00');
INSERT INTO project(project_status_type_id, manager_person_id, name, description, start_date, end_date, budget, spent) VALUES
  (1, 1, 'Blueman Industrial Site Architecture', 'Main website architecture for the Blueman Industrial Group', '2006-03-01', '2006-04-15', '2500.00', '4200.50');
INSERT INTO project(project_status_type_id, manager_person_id, name, description, start_date, end_date, budget, spent) VALUES
  (2, 7, 'ACME Payment System', 'Accounts Payable payment system for ACME Incorporated', '2005-08-15', '2005-10-20', '5124.67', '5175.30');

INSERT INTO team_member_project_assn (person_id, project_id) VALUES (2, 1);
INSERT INTO team_member_project_assn (person_id, project_id) VALUES (5, 1);
INSERT INTO team_member_project_assn (person_id, project_id) VALUES (6, 1);
INSERT INTO team_member_project_assn (person_id, project_id) VALUES (7, 1);
INSERT INTO team_member_project_assn (person_id, project_id) VALUES (8, 1);

INSERT INTO team_member_project_assn (person_id, project_id) VALUES (2, 2);
INSERT INTO team_member_project_assn (person_id, project_id) VALUES (4, 2);
INSERT INTO team_member_project_assn (person_id, project_id) VALUES (5, 2);
INSERT INTO team_member_project_assn (person_id, project_id) VALUES (7, 2);
INSERT INTO team_member_project_assn (person_id, project_id) VALUES (9, 2);
INSERT INTO team_member_project_assn (person_id, project_id) VALUES (10, 2);

INSERT INTO team_member_project_assn (person_id, project_id) VALUES (1, 3);
INSERT INTO team_member_project_assn (person_id, project_id) VALUES (4, 3);
INSERT INTO team_member_project_assn (person_id, project_id) VALUES (6, 3);
INSERT INTO team_member_project_assn (person_id, project_id) VALUES (8, 3);
INSERT INTO team_member_project_assn (person_id, project_id) VALUES (10, 3);

INSERT INTO team_member_project_assn (person_id, project_id) VALUES (1, 4);
INSERT INTO team_member_project_assn (person_id, project_id) VALUES (2, 4);
INSERT INTO team_member_project_assn (person_id, project_id) VALUES (3, 4);
INSERT INTO team_member_project_assn (person_id, project_id) VALUES (5, 4);
INSERT INTO team_member_project_assn (person_id, project_id) VALUES (8, 4);
INSERT INTO team_member_project_assn (person_id, project_id) VALUES (11, 4);
INSERT INTO team_member_project_assn (person_id, project_id) VALUES (12, 4);

INSERT INTO related_project_assn (project_id, child_project_id) VALUES(1, 3);
INSERT INTO related_project_assn (project_id, child_project_id) VALUES(1, 4);

INSERT INTO related_project_assn (project_id, child_project_id) VALUES(4, 1);

INSERT INTO address (person_id, street, city) VALUES(1, '1 Love Drive', 'Phoenix');
INSERT INTO address (person_id, street, city) VALUES(2, '2 Doves and a Pine Cone Dr.', 'Dallas');
INSERT INTO address (person_id, street, city) VALUES(3, '3 Gold Fish Pl.', 'New York');
INSERT INTO address (person_id, street, city) VALUES(3, '323 W QCubed', 'New York');
INSERT INTO address (person_id, street, city) VALUES(5, '22 Elm St', 'Palo Alto');
INSERT INTO address (person_id, street, city) VALUES(7, '1 Pine St', 'San Jose');
INSERT INTO address (person_id, street, city) VALUES(7, '421 Central Expw', 'Mountain View');

INSERT INTO milestone (project_id, name) VALUES (1, 'Milestone A');
INSERT INTO milestone (project_id, name) VALUES (1, 'Milestone B');
INSERT INTO milestone (project_id, name) VALUES (1, 'Milestone C');
INSERT INTO milestone (project_id, name) VALUES (2, 'Milestone D');
INSERT INTO milestone (project_id, name) VALUES (2, 'Milestone E');
INSERT INTO milestone (project_id, name) VALUES (3, 'Milestone F');
INSERT INTO milestone (project_id, name) VALUES (4, 'Milestone G');
INSERT INTO milestone (project_id, name) VALUES (4, 'Milestone H');
INSERT INTO milestone (project_id, name) VALUES (4, 'Milestone I');
INSERT INTO milestone (project_id, name) VALUES (4, 'Milestone J');
