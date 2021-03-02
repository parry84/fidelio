-- Write your SQL migration code in here
ALTER TABLE secrets 
ADD COLUMN sender TEXT DEFAULT NULL
ADD COLUMN failed_attempts_count INT DEFAULT 0 NOT NULL
ADD COLUMN notify_sender BOOLEAN DEFAULT false NOT NULL
ADD COLUMN view_count INT DEFAULT 0 NOT NULL
ADD COLUMN max_views INT DEFAULT 1 NOT NULL