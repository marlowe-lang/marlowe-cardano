--
-- PostgreSQL database dump
--

\restrict XtTZM5zFU5QqjveN2CymLAA76zOxbXZHJtfNf0y4iYVgIsKAD5lq8gMlvXSjNMR

-- Dumped from database version 17.7
-- Dumped by pg_dump version 17.7

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET transaction_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: marlowe; Type: SCHEMA; Schema: -; Owner: marlowe
--

CREATE SCHEMA marlowe;


ALTER SCHEMA marlowe OWNER TO marlowe;

--
-- Name: indexer_status_attr; Type: TYPE; Schema: marlowe; Owner: marlowe
--

CREATE TYPE marlowe.indexer_status_attr AS ENUM (
    'tip'
);


ALTER TYPE marlowe.indexer_status_attr OWNER TO marlowe;

--
-- Name: node_status_attr; Type: TYPE; Schema: marlowe; Owner: marlowe
--

CREATE TYPE marlowe.node_status_attr AS ENUM (
    'tip',
    'eraHistory'
);


ALTER TYPE marlowe.node_status_attr OWNER TO marlowe;

SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: applytx; Type: TABLE; Schema: marlowe; Owner: marlowe
--

CREATE TABLE marlowe.applytx (
    txid bytea NOT NULL,
    createtxid bytea NOT NULL,
    createtxix smallint NOT NULL,
    blockid bytea NOT NULL,
    invalidbefore timestamp without time zone NOT NULL,
    invalidhereafter timestamp without time zone NOT NULL,
    metadata bytea,
    inputtxid bytea NOT NULL,
    inputtxix smallint NOT NULL,
    inputs bytea NOT NULL,
    outputtxix smallint,
    slotno bigint NOT NULL,
    blockno bigint NOT NULL
);


ALTER TABLE marlowe.applytx OWNER TO marlowe;

--
-- Name: block; Type: TABLE; Schema: marlowe; Owner: marlowe
--

CREATE TABLE marlowe.block (
    id bytea NOT NULL,
    slotno bigint NOT NULL,
    blockno bigint NOT NULL
);


ALTER TABLE marlowe.block OWNER TO marlowe;

--
-- Name: contracttxout; Type: TABLE; Schema: marlowe; Owner: marlowe
--

CREATE TABLE marlowe.contracttxout (
    txid bytea NOT NULL,
    txix smallint NOT NULL,
    blockid bytea NOT NULL,
    payoutscripthash bytea NOT NULL,
    contract bytea NOT NULL,
    state bytea NOT NULL,
    rolescurrency bytea NOT NULL
);


ALTER TABLE marlowe.contracttxout OWNER TO marlowe;

--
-- Name: contracttxoutpartyaddress; Type: TABLE; Schema: marlowe; Owner: marlowe
--

CREATE TABLE marlowe.contracttxoutpartyaddress (
    address bytea NOT NULL,
    txid bytea NOT NULL,
    txix smallint NOT NULL,
    createtxid bytea NOT NULL,
    createtxix smallint NOT NULL
);


ALTER TABLE marlowe.contracttxoutpartyaddress OWNER TO marlowe;

--
-- Name: contracttxoutpartyrole; Type: TABLE; Schema: marlowe; Owner: marlowe
--

CREATE TABLE marlowe.contracttxoutpartyrole (
    rolescurrency bytea NOT NULL,
    role bytea NOT NULL,
    txid bytea NOT NULL,
    txix smallint NOT NULL,
    createtxid bytea NOT NULL,
    createtxix smallint NOT NULL
);


ALTER TABLE marlowe.contracttxoutpartyrole OWNER TO marlowe;

--
-- Name: contracttxouttag; Type: TABLE; Schema: marlowe; Owner: marlowe
--

CREATE TABLE marlowe.contracttxouttag (
    tag text NOT NULL,
    txid bytea NOT NULL,
    txix smallint NOT NULL
);


ALTER TABLE marlowe.contracttxouttag OWNER TO marlowe;

--
-- Name: createtxout; Type: TABLE; Schema: marlowe; Owner: marlowe
--

CREATE TABLE marlowe.createtxout (
    txid bytea NOT NULL,
    txix smallint NOT NULL,
    blockid bytea NOT NULL,
    metadata bytea,
    slotno bigint NOT NULL,
    blockno bigint NOT NULL
);


ALTER TABLE marlowe.createtxout OWNER TO marlowe;

--
-- Name: indexer_status; Type: TABLE; Schema: marlowe; Owner: marlowe
--

CREATE TABLE marlowe.indexer_status (
    attr marlowe.indexer_status_attr NOT NULL,
    value bytea
);


ALTER TABLE marlowe.indexer_status OWNER TO marlowe;

--
-- Name: invalidapplytx; Type: TABLE; Schema: marlowe; Owner: marlowe
--

CREATE TABLE marlowe.invalidapplytx (
    txid bytea NOT NULL,
    inputtxid bytea NOT NULL,
    inputtxix smallint NOT NULL,
    blockid bytea NOT NULL,
    error text NOT NULL
);


ALTER TABLE marlowe.invalidapplytx OWNER TO marlowe;

--
-- Name: node_status; Type: TABLE; Schema: marlowe; Owner: marlowe
--

CREATE TABLE marlowe.node_status (
    attr marlowe.node_status_attr NOT NULL,
    value bytea
);


ALTER TABLE marlowe.node_status OWNER TO marlowe;

--
-- Name: payouttxout; Type: TABLE; Schema: marlowe; Owner: marlowe
--

CREATE TABLE marlowe.payouttxout (
    txid bytea NOT NULL,
    txix smallint NOT NULL,
    blockid bytea NOT NULL,
    rolescurrency bytea NOT NULL,
    role bytea NOT NULL
);


ALTER TABLE marlowe.payouttxout OWNER TO marlowe;

--
-- Name: rollbackblock; Type: TABLE; Schema: marlowe; Owner: marlowe
--

CREATE TABLE marlowe.rollbackblock (
    fromblock bytea NOT NULL,
    toblock bytea NOT NULL,
    toslotno bigint NOT NULL
);


ALTER TABLE marlowe.rollbackblock OWNER TO marlowe;

--
-- Name: txout; Type: TABLE; Schema: marlowe; Owner: marlowe
--

CREATE TABLE marlowe.txout (
    txid bytea NOT NULL,
    txix smallint NOT NULL,
    blockid bytea NOT NULL,
    address bytea NOT NULL,
    lovelace bigint NOT NULL
);


ALTER TABLE marlowe.txout OWNER TO marlowe;

--
-- Name: txoutasset; Type: TABLE; Schema: marlowe; Owner: marlowe
--

CREATE TABLE marlowe.txoutasset (
    txid bytea NOT NULL,
    txix smallint NOT NULL,
    blockid bytea NOT NULL,
    policyid bytea NOT NULL,
    name bytea NOT NULL,
    quantity bigint NOT NULL
);


ALTER TABLE marlowe.txoutasset OWNER TO marlowe;

--
-- Name: withdrawaltxin; Type: TABLE; Schema: marlowe; Owner: marlowe
--

CREATE TABLE marlowe.withdrawaltxin (
    txid bytea NOT NULL,
    blockid bytea NOT NULL,
    payouttxid bytea NOT NULL,
    payouttxix smallint NOT NULL,
    createtxid bytea NOT NULL,
    createtxix smallint NOT NULL,
    slotno bigint NOT NULL,
    blockno bigint NOT NULL
);


ALTER TABLE marlowe.withdrawaltxin OWNER TO marlowe;

--
-- Name: applytx applytx_inputtxid_inputtxix_key; Type: CONSTRAINT; Schema: marlowe; Owner: marlowe
--

ALTER TABLE ONLY marlowe.applytx
    ADD CONSTRAINT applytx_inputtxid_inputtxix_key UNIQUE (inputtxid, inputtxix);


--
-- Name: applytx applytx_pkey; Type: CONSTRAINT; Schema: marlowe; Owner: marlowe
--

ALTER TABLE ONLY marlowe.applytx
    ADD CONSTRAINT applytx_pkey PRIMARY KEY (txid);


--
-- Name: block block_pkey; Type: CONSTRAINT; Schema: marlowe; Owner: marlowe
--

ALTER TABLE ONLY marlowe.block
    ADD CONSTRAINT block_pkey PRIMARY KEY (id);


--
-- Name: contracttxout contracttxout_pkey; Type: CONSTRAINT; Schema: marlowe; Owner: marlowe
--

ALTER TABLE ONLY marlowe.contracttxout
    ADD CONSTRAINT contracttxout_pkey PRIMARY KEY (txid, txix);


--
-- Name: contracttxoutpartyaddress contracttxoutpartyaddress_pkey; Type: CONSTRAINT; Schema: marlowe; Owner: marlowe
--

ALTER TABLE ONLY marlowe.contracttxoutpartyaddress
    ADD CONSTRAINT contracttxoutpartyaddress_pkey PRIMARY KEY (address, txid, txix);


--
-- Name: contracttxoutpartyrole contracttxoutpartyrole_pkey; Type: CONSTRAINT; Schema: marlowe; Owner: marlowe
--

ALTER TABLE ONLY marlowe.contracttxoutpartyrole
    ADD CONSTRAINT contracttxoutpartyrole_pkey PRIMARY KEY (rolescurrency, role, txid, txix);


--
-- Name: contracttxouttag contracttxouttag_pkey; Type: CONSTRAINT; Schema: marlowe; Owner: marlowe
--

ALTER TABLE ONLY marlowe.contracttxouttag
    ADD CONSTRAINT contracttxouttag_pkey PRIMARY KEY (tag, txid, txix);


--
-- Name: createtxout createtxout_pkey; Type: CONSTRAINT; Schema: marlowe; Owner: marlowe
--

ALTER TABLE ONLY marlowe.createtxout
    ADD CONSTRAINT createtxout_pkey PRIMARY KEY (txid, txix);


--
-- Name: indexer_status indexer_status_pkey; Type: CONSTRAINT; Schema: marlowe; Owner: marlowe
--

ALTER TABLE ONLY marlowe.indexer_status
    ADD CONSTRAINT indexer_status_pkey PRIMARY KEY (attr);


--
-- Name: invalidapplytx invalidapplytx_pkey; Type: CONSTRAINT; Schema: marlowe; Owner: marlowe
--

ALTER TABLE ONLY marlowe.invalidapplytx
    ADD CONSTRAINT invalidapplytx_pkey PRIMARY KEY (txid);


--
-- Name: node_status node_status_pkey; Type: CONSTRAINT; Schema: marlowe; Owner: marlowe
--

ALTER TABLE ONLY marlowe.node_status
    ADD CONSTRAINT node_status_pkey PRIMARY KEY (attr);


--
-- Name: payouttxout payouttxout_pkey; Type: CONSTRAINT; Schema: marlowe; Owner: marlowe
--

ALTER TABLE ONLY marlowe.payouttxout
    ADD CONSTRAINT payouttxout_pkey PRIMARY KEY (txid, txix);


--
-- Name: rollbackblock rollbackblock_pkey; Type: CONSTRAINT; Schema: marlowe; Owner: marlowe
--

ALTER TABLE ONLY marlowe.rollbackblock
    ADD CONSTRAINT rollbackblock_pkey PRIMARY KEY (fromblock);


--
-- Name: txout txout_pkey; Type: CONSTRAINT; Schema: marlowe; Owner: marlowe
--

ALTER TABLE ONLY marlowe.txout
    ADD CONSTRAINT txout_pkey PRIMARY KEY (txid, txix);


--
-- Name: txoutasset txoutasset_pkey; Type: CONSTRAINT; Schema: marlowe; Owner: marlowe
--

ALTER TABLE ONLY marlowe.txoutasset
    ADD CONSTRAINT txoutasset_pkey PRIMARY KEY (txid, txix, policyid, name);


--
-- Name: withdrawaltxin withdrawaltxin_pkey; Type: CONSTRAINT; Schema: marlowe; Owner: marlowe
--

ALTER TABLE ONLY marlowe.withdrawaltxin
    ADD CONSTRAINT withdrawaltxin_pkey PRIMARY KEY (payouttxid, payouttxix);


--
-- Name: applytx_blockid; Type: INDEX; Schema: marlowe; Owner: marlowe
--

CREATE INDEX applytx_blockid ON marlowe.applytx USING btree (blockid);


--
-- Name: applytx_createtxid_createtxix; Type: INDEX; Schema: marlowe; Owner: marlowe
--

CREATE INDEX applytx_createtxid_createtxix ON marlowe.applytx USING btree (createtxid, createtxix);


--
-- Name: applytx_inputtxid_inputtxix; Type: INDEX; Schema: marlowe; Owner: marlowe
--

CREATE INDEX applytx_inputtxid_inputtxix ON marlowe.applytx USING btree (inputtxid, inputtxix);


--
-- Name: applytx_slotno; Type: INDEX; Schema: marlowe; Owner: marlowe
--

CREATE INDEX applytx_slotno ON marlowe.applytx USING btree (slotno);


--
-- Name: applytx_slotno_txid_txix; Type: INDEX; Schema: marlowe; Owner: marlowe
--

CREATE INDEX applytx_slotno_txid_txix ON marlowe.applytx USING btree (slotno, txid);


--
-- Name: applytx_txid_outputtxix; Type: INDEX; Schema: marlowe; Owner: marlowe
--

CREATE INDEX applytx_txid_outputtxix ON marlowe.applytx USING btree (txid, outputtxix);


--
-- Name: block_slotno; Type: INDEX; Schema: marlowe; Owner: marlowe
--

CREATE INDEX block_slotno ON marlowe.block USING btree (slotno);


--
-- Name: block_slotno_id; Type: INDEX; Schema: marlowe; Owner: marlowe
--

CREATE INDEX block_slotno_id ON marlowe.block USING btree (slotno, id);


--
-- Name: contracttxout_blockid; Type: INDEX; Schema: marlowe; Owner: marlowe
--

CREATE INDEX contracttxout_blockid ON marlowe.contracttxout USING btree (blockid);


--
-- Name: contracttxout_rolescurrency; Type: INDEX; Schema: marlowe; Owner: marlowe
--

CREATE INDEX contracttxout_rolescurrency ON marlowe.contracttxout USING btree (rolescurrency);


--
-- Name: contracttxoutpartyaddress_tag; Type: INDEX; Schema: marlowe; Owner: marlowe
--

CREATE INDEX contracttxoutpartyaddress_tag ON marlowe.contracttxoutpartyaddress USING btree (address);


--
-- Name: contracttxoutpartyrole_tag; Type: INDEX; Schema: marlowe; Owner: marlowe
--

CREATE INDEX contracttxoutpartyrole_tag ON marlowe.contracttxoutpartyrole USING btree (rolescurrency, role);


--
-- Name: contracttxouttag_tag; Type: INDEX; Schema: marlowe; Owner: marlowe
--

CREATE INDEX contracttxouttag_tag ON marlowe.contracttxouttag USING btree (tag);


--
-- Name: createtxout_blockid; Type: INDEX; Schema: marlowe; Owner: marlowe
--

CREATE INDEX createtxout_blockid ON marlowe.createtxout USING btree (blockid);


--
-- Name: createtxout_slotno; Type: INDEX; Schema: marlowe; Owner: marlowe
--

CREATE INDEX createtxout_slotno ON marlowe.createtxout USING btree (slotno);


--
-- Name: createtxout_slotno_txid_txix; Type: INDEX; Schema: marlowe; Owner: marlowe
--

CREATE INDEX createtxout_slotno_txid_txix ON marlowe.createtxout USING btree (slotno, txid, txix);


--
-- Name: invalidapplytx_blockid; Type: INDEX; Schema: marlowe; Owner: marlowe
--

CREATE INDEX invalidapplytx_blockid ON marlowe.invalidapplytx USING btree (blockid);


--
-- Name: invalidapplytx_inputtxid_inputtxix; Type: INDEX; Schema: marlowe; Owner: marlowe
--

CREATE INDEX invalidapplytx_inputtxid_inputtxix ON marlowe.invalidapplytx USING btree (inputtxid, inputtxix);


--
-- Name: payouttxout_blockid; Type: INDEX; Schema: marlowe; Owner: marlowe
--

CREATE INDEX payouttxout_blockid ON marlowe.payouttxout USING btree (blockid);


--
-- Name: payouttxout_rolescurrency; Type: INDEX; Schema: marlowe; Owner: marlowe
--

CREATE INDEX payouttxout_rolescurrency ON marlowe.payouttxout USING btree (rolescurrency);


--
-- Name: payouttxout_rolescurrency_role; Type: INDEX; Schema: marlowe; Owner: marlowe
--

CREATE INDEX payouttxout_rolescurrency_role ON marlowe.payouttxout USING btree (rolescurrency, role);


--
-- Name: payouttxout_txid; Type: INDEX; Schema: marlowe; Owner: marlowe
--

CREATE INDEX payouttxout_txid ON marlowe.payouttxout USING btree (txid);


--
-- Name: rollbackblock_toblock; Type: INDEX; Schema: marlowe; Owner: marlowe
--

CREATE INDEX rollbackblock_toblock ON marlowe.rollbackblock USING btree (toblock);


--
-- Name: rollbackblock_toslotno; Type: INDEX; Schema: marlowe; Owner: marlowe
--

CREATE INDEX rollbackblock_toslotno ON marlowe.rollbackblock USING btree (toslotno);


--
-- Name: txout_address; Type: INDEX; Schema: marlowe; Owner: marlowe
--

CREATE INDEX txout_address ON marlowe.txout USING btree (((md5(address))::uuid));


--
-- Name: txout_blockid; Type: INDEX; Schema: marlowe; Owner: marlowe
--

CREATE INDEX txout_blockid ON marlowe.txout USING btree (blockid);


--
-- Name: txoutasset_blockid; Type: INDEX; Schema: marlowe; Owner: marlowe
--

CREATE INDEX txoutasset_blockid ON marlowe.txoutasset USING btree (blockid);


--
-- Name: txoutasset_txid_txix; Type: INDEX; Schema: marlowe; Owner: marlowe
--

CREATE INDEX txoutasset_txid_txix ON marlowe.txoutasset USING btree (txid, txix);


--
-- Name: withdrawaltxin_createtxid_createtxix; Type: INDEX; Schema: marlowe; Owner: marlowe
--

CREATE INDEX withdrawaltxin_createtxid_createtxix ON marlowe.withdrawaltxin USING btree (createtxid, createtxix);


--
-- Name: withdrawaltxin_slotno; Type: INDEX; Schema: marlowe; Owner: marlowe
--

CREATE INDEX withdrawaltxin_slotno ON marlowe.withdrawaltxin USING btree (slotno);


--
-- Name: withdrawaltxin_slotno_txid_txix; Type: INDEX; Schema: marlowe; Owner: marlowe
--

CREATE INDEX withdrawaltxin_slotno_txid_txix ON marlowe.withdrawaltxin USING btree (slotno, txid);


--
-- Name: withdrawtxin_blockid; Type: INDEX; Schema: marlowe; Owner: marlowe
--

CREATE INDEX withdrawtxin_blockid ON marlowe.withdrawaltxin USING btree (blockid);


--
-- Name: withdrawtxin_txid; Type: INDEX; Schema: marlowe; Owner: marlowe
--

CREATE INDEX withdrawtxin_txid ON marlowe.withdrawaltxin USING btree (txid);


--
-- Name: applytx applytx_blockid_fkey; Type: FK CONSTRAINT; Schema: marlowe; Owner: marlowe
--

ALTER TABLE ONLY marlowe.applytx
    ADD CONSTRAINT applytx_blockid_fkey FOREIGN KEY (blockid) REFERENCES marlowe.block(id) ON DELETE CASCADE;


--
-- Name: applytx applytx_createtxid_createtxix_fkey; Type: FK CONSTRAINT; Schema: marlowe; Owner: marlowe
--

ALTER TABLE ONLY marlowe.applytx
    ADD CONSTRAINT applytx_createtxid_createtxix_fkey FOREIGN KEY (createtxid, createtxix) REFERENCES marlowe.createtxout(txid, txix);


--
-- Name: applytx applytx_inputtxid_inputtxix_fkey; Type: FK CONSTRAINT; Schema: marlowe; Owner: marlowe
--

ALTER TABLE ONLY marlowe.applytx
    ADD CONSTRAINT applytx_inputtxid_inputtxix_fkey FOREIGN KEY (inputtxid, inputtxix) REFERENCES marlowe.contracttxout(txid, txix);


--
-- Name: applytx applytx_txid_outputtxix_fkey; Type: FK CONSTRAINT; Schema: marlowe; Owner: marlowe
--

ALTER TABLE ONLY marlowe.applytx
    ADD CONSTRAINT applytx_txid_outputtxix_fkey FOREIGN KEY (txid, outputtxix) REFERENCES marlowe.contracttxout(txid, txix);


--
-- Name: contracttxout contracttxout_blockid_fkey; Type: FK CONSTRAINT; Schema: marlowe; Owner: marlowe
--

ALTER TABLE ONLY marlowe.contracttxout
    ADD CONSTRAINT contracttxout_blockid_fkey FOREIGN KEY (blockid) REFERENCES marlowe.block(id) ON DELETE CASCADE;


--
-- Name: contracttxout contracttxout_txid_txix_fkey; Type: FK CONSTRAINT; Schema: marlowe; Owner: marlowe
--

ALTER TABLE ONLY marlowe.contracttxout
    ADD CONSTRAINT contracttxout_txid_txix_fkey FOREIGN KEY (txid, txix) REFERENCES marlowe.txout(txid, txix);


--
-- Name: contracttxoutpartyaddress contracttxoutpartyaddress_createtxid_createtxix_fkey; Type: FK CONSTRAINT; Schema: marlowe; Owner: marlowe
--

ALTER TABLE ONLY marlowe.contracttxoutpartyaddress
    ADD CONSTRAINT contracttxoutpartyaddress_createtxid_createtxix_fkey FOREIGN KEY (createtxid, createtxix) REFERENCES marlowe.createtxout(txid, txix) ON DELETE CASCADE;


--
-- Name: contracttxoutpartyaddress contracttxoutpartyaddress_txid_txix_fkey; Type: FK CONSTRAINT; Schema: marlowe; Owner: marlowe
--

ALTER TABLE ONLY marlowe.contracttxoutpartyaddress
    ADD CONSTRAINT contracttxoutpartyaddress_txid_txix_fkey FOREIGN KEY (txid, txix) REFERENCES marlowe.contracttxout(txid, txix) ON DELETE CASCADE;


--
-- Name: contracttxoutpartyrole contracttxoutpartyrole_createtxid_createtxix_fkey; Type: FK CONSTRAINT; Schema: marlowe; Owner: marlowe
--

ALTER TABLE ONLY marlowe.contracttxoutpartyrole
    ADD CONSTRAINT contracttxoutpartyrole_createtxid_createtxix_fkey FOREIGN KEY (createtxid, createtxix) REFERENCES marlowe.createtxout(txid, txix) ON DELETE CASCADE;


--
-- Name: contracttxoutpartyrole contracttxoutpartyrole_txid_txix_fkey; Type: FK CONSTRAINT; Schema: marlowe; Owner: marlowe
--

ALTER TABLE ONLY marlowe.contracttxoutpartyrole
    ADD CONSTRAINT contracttxoutpartyrole_txid_txix_fkey FOREIGN KEY (txid, txix) REFERENCES marlowe.contracttxout(txid, txix) ON DELETE CASCADE;


--
-- Name: contracttxouttag contracttxouttag_txid_txix_fkey; Type: FK CONSTRAINT; Schema: marlowe; Owner: marlowe
--

ALTER TABLE ONLY marlowe.contracttxouttag
    ADD CONSTRAINT contracttxouttag_txid_txix_fkey FOREIGN KEY (txid, txix) REFERENCES marlowe.contracttxout(txid, txix) ON DELETE CASCADE;


--
-- Name: createtxout createtxout_blockid_fkey; Type: FK CONSTRAINT; Schema: marlowe; Owner: marlowe
--

ALTER TABLE ONLY marlowe.createtxout
    ADD CONSTRAINT createtxout_blockid_fkey FOREIGN KEY (blockid) REFERENCES marlowe.block(id) ON DELETE CASCADE;


--
-- Name: createtxout createtxout_txid_txix_fkey; Type: FK CONSTRAINT; Schema: marlowe; Owner: marlowe
--

ALTER TABLE ONLY marlowe.createtxout
    ADD CONSTRAINT createtxout_txid_txix_fkey FOREIGN KEY (txid, txix) REFERENCES marlowe.contracttxout(txid, txix);


--
-- Name: invalidapplytx invalidapplytx_blockid_fkey; Type: FK CONSTRAINT; Schema: marlowe; Owner: marlowe
--

ALTER TABLE ONLY marlowe.invalidapplytx
    ADD CONSTRAINT invalidapplytx_blockid_fkey FOREIGN KEY (blockid) REFERENCES marlowe.block(id) ON DELETE CASCADE;


--
-- Name: invalidapplytx invalidapplytx_inputtxid_inputtxix_fkey; Type: FK CONSTRAINT; Schema: marlowe; Owner: marlowe
--

ALTER TABLE ONLY marlowe.invalidapplytx
    ADD CONSTRAINT invalidapplytx_inputtxid_inputtxix_fkey FOREIGN KEY (inputtxid, inputtxix) REFERENCES marlowe.contracttxout(txid, txix);


--
-- Name: payouttxout payouttxout_blockid_fkey; Type: FK CONSTRAINT; Schema: marlowe; Owner: marlowe
--

ALTER TABLE ONLY marlowe.payouttxout
    ADD CONSTRAINT payouttxout_blockid_fkey FOREIGN KEY (blockid) REFERENCES marlowe.block(id) ON DELETE CASCADE;


--
-- Name: payouttxout payouttxout_txid_fkey; Type: FK CONSTRAINT; Schema: marlowe; Owner: marlowe
--

ALTER TABLE ONLY marlowe.payouttxout
    ADD CONSTRAINT payouttxout_txid_fkey FOREIGN KEY (txid) REFERENCES marlowe.applytx(txid);


--
-- Name: payouttxout payouttxout_txid_txix_fkey; Type: FK CONSTRAINT; Schema: marlowe; Owner: marlowe
--

ALTER TABLE ONLY marlowe.payouttxout
    ADD CONSTRAINT payouttxout_txid_txix_fkey FOREIGN KEY (txid, txix) REFERENCES marlowe.txout(txid, txix);


--
-- Name: rollbackblock rollbackblock_toblock_fkey; Type: FK CONSTRAINT; Schema: marlowe; Owner: marlowe
--

ALTER TABLE ONLY marlowe.rollbackblock
    ADD CONSTRAINT rollbackblock_toblock_fkey FOREIGN KEY (toblock) REFERENCES marlowe.block(id);


--
-- Name: txout txout_blockid_fkey; Type: FK CONSTRAINT; Schema: marlowe; Owner: marlowe
--

ALTER TABLE ONLY marlowe.txout
    ADD CONSTRAINT txout_blockid_fkey FOREIGN KEY (blockid) REFERENCES marlowe.block(id) ON DELETE CASCADE;


--
-- Name: txoutasset txoutasset_blockid_fkey; Type: FK CONSTRAINT; Schema: marlowe; Owner: marlowe
--

ALTER TABLE ONLY marlowe.txoutasset
    ADD CONSTRAINT txoutasset_blockid_fkey FOREIGN KEY (blockid) REFERENCES marlowe.block(id) ON DELETE CASCADE;


--
-- Name: txoutasset txoutasset_txid_txix_fkey; Type: FK CONSTRAINT; Schema: marlowe; Owner: marlowe
--

ALTER TABLE ONLY marlowe.txoutasset
    ADD CONSTRAINT txoutasset_txid_txix_fkey FOREIGN KEY (txid, txix) REFERENCES marlowe.txout(txid, txix);


--
-- Name: withdrawaltxin withdrawaltxin_blockid_fkey; Type: FK CONSTRAINT; Schema: marlowe; Owner: marlowe
--

ALTER TABLE ONLY marlowe.withdrawaltxin
    ADD CONSTRAINT withdrawaltxin_blockid_fkey FOREIGN KEY (blockid) REFERENCES marlowe.block(id) ON DELETE CASCADE;


--
-- Name: withdrawaltxin withdrawaltxin_createtxid_createtxix_fkey; Type: FK CONSTRAINT; Schema: marlowe; Owner: marlowe
--

ALTER TABLE ONLY marlowe.withdrawaltxin
    ADD CONSTRAINT withdrawaltxin_createtxid_createtxix_fkey FOREIGN KEY (createtxid, createtxix) REFERENCES marlowe.createtxout(txid, txix);


--
-- Name: withdrawaltxin withdrawaltxin_payouttxid_payouttxix_fkey; Type: FK CONSTRAINT; Schema: marlowe; Owner: marlowe
--

ALTER TABLE ONLY marlowe.withdrawaltxin
    ADD CONSTRAINT withdrawaltxin_payouttxid_payouttxix_fkey FOREIGN KEY (payouttxid, payouttxix) REFERENCES marlowe.payouttxout(txid, txix);


--
-- PostgreSQL database dump complete
--

\unrestrict XtTZM5zFU5QqjveN2CymLAA76zOxbXZHJtfNf0y4iYVgIsKAD5lq8gMlvXSjNMR

