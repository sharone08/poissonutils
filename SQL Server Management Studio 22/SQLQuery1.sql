CREATE DATABASE JardinEntreprise;
GO

USE JardinEntreprise;
GO

CREATE TABLE Clients (
  id INT PRIMARY KEY IDENTITY(1,1),
  nom NVARCHAR(100) NOT NULL,
  email NVARCHAR(100) NOT NULL,
  telephone NVARCHAR(20)
);

CREATE TABLE Services (
  id INT PRIMARY KEY IDENTITY(1,1),
  nom NVARCHAR(100) NOT NULL,
  description NVARCHAR(MAX)
);

CREATE TABLE Devis (
  id INT PRIMARY KEY IDENTITY(1,1),
  client_id INT NOT NULL,
  service_id INT NOT NULL,
  date DATE NOT NULL,
  montant DECIMAL(10,2) NOT NULL,
  FOREIGN KEY (client_id) REFERENCES Clients(id),
  FOREIGN KEY (service_id) REFERENCES Services(id)
);

CREATE TABLE MessagesContact (
  id INT PRIMARY KEY IDENTITY(1,1),
  client_id INT NOT NULL,
  message NVARCHAR(MAX) NOT NULL,
  date DATETIME DEFAULT GETDATE(),
  FOREIGN KEY (client_id) REFERENCES Clients(id)
);
